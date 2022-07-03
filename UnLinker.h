#pragma once

//#define PACKAGE_FILE_TAG 0x9E2A83C1
constexpr INT PACKAGE_FILE_TAG = 0x9E2A83C1;
constexpr INT PACKAGE_FILE_VERSION = 68;

struct FObjectExport
{
	// Variables.
	INT         ClassIndex;		// Persistent.
	INT         SuperIndex;		// Persistent (for UStruct-derived objects only).
	INT			PackageIndex;	// Persistent.
	FName		ObjectName;		// Persistent.
	DWORD		ObjectFlags;	// Persistent.
	INT         SerialSize;		// Persistent.
	INT         SerialOffset;	// Persistent (for checking only).

	// Functions.
	FObjectExport()
	{}
	friend FArchive& operator<<(FArchive& Ar, FObjectExport& E)
	{
		Ar << AR_INDEX(E.ClassIndex);
		Ar << AR_INDEX(E.SuperIndex);
		Ar << E.PackageIndex;
		Ar << E.ObjectName;
		Ar << E.ObjectFlags;
		Ar << AR_INDEX(E.SerialSize);

		if (E.SerialSize)
			Ar << AR_INDEX(E.SerialOffset);

		return Ar;
	}
};

//
// Information about an imported object.
//
struct FObjectImport
{
	// Variables.
	FName			ClassPackage;	// Persistent.
	FName			ClassName;		// Persistent.
	INT				PackageIndex;	// Persistent.
	FName			ObjectName;		// Persistent.
	class LinkerLoad* SourceLinker;	// Internal.
	INT             SourceIndex;	// Internal.

	// Functions.
	FObjectImport()
	{}
	friend FArchive& operator<<(FArchive& Ar, FObjectImport& I)
	{
		Ar << I.ClassPackage << I.ClassName;
		Ar << I.PackageIndex;
		Ar << I.ObjectName;
		if (Ar.IsLoading())
			I.SourceIndex = INDEX_NONE;
		return Ar;
	}
};

class FGuid
{
public:
	DWORD A, B, C, D;
	FGuid()
	{}
	FGuid(DWORD InA, DWORD InB, DWORD InC, DWORD InD)
		: A(InA), B(InB), C(InC), D(InD)
	{}
	friend UBOOL operator==(const FGuid& X, const FGuid& Y)
	{
		return X.A == Y.A && X.B == Y.B && X.C == Y.C && X.D == Y.D;
	}
	friend UBOOL operator!=(const FGuid& X, const FGuid& Y)
	{
		return X.A != Y.A || X.B != Y.B || X.C != Y.C || X.D != Y.D;
	}
	friend FArchive& operator<<(FArchive& Ar, FGuid& G)
	{
		return Ar << G.A << G.B << G.C << G.D;
	}
	FString String() const
	{
		return FString::Printf(TEXT("%08X%08X%08X%08X"), A, B, C, D);
	}
};
struct FGenerationInfo
{
	INT ExportCount, NameCount;
	FGenerationInfo(INT InExportCount, INT InNameCount)
		: ExportCount(InExportCount), NameCount(InNameCount)
	{}
	friend FArchive& operator<<(FArchive& Ar, FGenerationInfo& Info)
	{
		return Ar << Info.ExportCount << Info.NameCount;
	}
};

struct FPackageFileSummary
{
	// Variables.
	INT		Tag;
	_WORD	FileVersion; // CDH changed from INT to _WORD
	_WORD	LicenseeVersion; // CDH
	DWORD	PackageFlags;
	INT		NameCount, NameOffset;
	INT		ExportCount, ExportOffset;
	INT     ImportCount, ImportOffset;
	FGuid	Guid;
	TArray<FGenerationInfo> Generations;

	// Constructor.
	FPackageFileSummary()
	{
		appMemzero(this, sizeof(*this));
	}

	// Serializer.
	friend FArchive& operator<<(FArchive& Ar, FPackageFileSummary& Sum)
	{
		Ar << Sum.Tag;
		if (Sum.Tag != PACKAGE_FILE_TAG)
			return Ar;
		Ar << Sum.FileVersion;
		Ar << Sum.LicenseeVersion; // CDH... storing licensee version in upper word, initial version is zero
		Ar << Sum.PackageFlags;
		Ar << Sum.NameCount << Sum.NameOffset;
		Ar << Sum.ExportCount << Sum.ExportOffset;
		Ar << Sum.ImportCount << Sum.ImportOffset;
		if (Sum.FileVersion >= 68)
		{
			INT GenerationCount = Sum.Generations.Num();
			Ar << Sum.Guid << GenerationCount;
			//!!67 had: return
			if (Ar.IsLoading())
				Sum.Generations = TArray<FGenerationInfo>(GenerationCount);
			for (INT i = 0; (i < GenerationCount && !Ar.IsError()); i++)
				Ar << Sum.Generations(i);
		}
		else //oldver
		{
			INT HeritageCount, HeritageOffset;
			Ar << HeritageCount << HeritageOffset;
			INT Saved = Ar.Tell();
			if (HeritageCount)
			{
				Ar.Seek(HeritageOffset);
				for (INT i = 0; (i < HeritageCount && !Ar.IsError()); i++)
					Ar << Sum.Guid;
			}
			Ar.Seek(Saved);
			if (Ar.IsLoading())
			{
				Sum.Generations.Empty(1);
				new(Sum.Generations)FGenerationInfo(Sum.ExportCount, Sum.NameCount);
			}
		}

		return Ar;
	}
};

struct FPropertyTag
{
	// Variables.
	BYTE	Type;		// Type of property, 0=end.
	BYTE	Info;		// Packed info byte.
	FName	Name;		// Name of property.
	FName	ItemName;	// Struct name if UStructProperty.
	INT		Size;       // Property size.
	INT		ArrayIndex;	// Index if an array; else 0.

	// Constructors.
	FPropertyTag()
	{}

	// Serializer.
	friend FArchive& operator<<(FArchive& Ar, FPropertyTag& Tag)
	{
		static TCHAR PrevTag[NAME_SIZE] = TEXT("");
		BYTE SizeByte;
		_WORD SizeWord;
		INT SizeInt;

		// Name.
		Ar << Tag.Name;
		if (Tag.Name == NAME_None)
			return Ar;
		appStrcpy(PrevTag, *Tag.Name);

		// Packed info byte:
		// Bit 0..3 = raw type.
		// Bit 4..6 = serialized size: [1 2 4 12 16 byte word int].
		// Bit 7    = array flag.
		Ar << Tag.Info;
		Tag.Type = Tag.Info & 0x0f;
		if (Tag.Type == NAME_StructProperty)
			Ar << Tag.ItemName;
		switch (Tag.Info & 0x70)
		{
		case 0x00:
			Tag.Size = 1;
			break;
		case 0x10:
			Tag.Size = 2;
			break;
		case 0x20:
			Tag.Size = 4;
			break;
		case 0x30:
			Tag.Size = 12;
			break;
		case 0x40:
			Tag.Size = 16;
			break;
		case 0x50:
			SizeByte = Tag.Size;
			Ar << SizeByte;
			Tag.Size = SizeByte;
			break;
		case 0x60:
			SizeWord = Tag.Size;
			Ar << SizeWord;
			Tag.Size = SizeWord;
			break;
		case 0x70:
			SizeInt = Tag.Size;
			Ar << SizeInt;
			Tag.Size = SizeInt;
			break;
		}
		if ((Tag.Info & 0x80) && Tag.Type != NAME_BoolProperty)
		{
			BYTE B
				= (Tag.ArrayIndex <= 127) ? (Tag.ArrayIndex)
				: (Tag.ArrayIndex <= 16383) ? (Tag.ArrayIndex >> 8) + 0x80
				: (Tag.ArrayIndex >> 24) + 0xC0;
			Ar << B;
			if ((B & 0x80) == 0)
			{
				Tag.ArrayIndex = B;
			}
			else if ((B & 0xC0) == 0x80)
			{
				BYTE C = Tag.ArrayIndex & 255;
				Ar << C;
				Tag.ArrayIndex = ((INT)(B & 0x7F) << 8) + ((INT)C);
			}
			else
			{
				BYTE C = Tag.ArrayIndex >> 16;
				BYTE D = Tag.ArrayIndex >> 8;
				BYTE E = Tag.ArrayIndex;
				Ar << C << D << E;
				Tag.ArrayIndex = ((INT)(B & 0x3F) << 24) + ((INT)C << 16) + ((INT)D << 8) + ((INT)E);
			}
		}
		else Tag.ArrayIndex = 0;
		return Ar;
	}

	// Property serializer.
	/*void SerializeTaggedProperty(FArchive& Ar, UProperty* Property, BYTE* Value)
	{
		if (Property->GetClass() == UBoolProperty::StaticClass())
		{
			UBoolProperty* Bool = (UBoolProperty*)Property;
			check(Bool->BitMask != 0);
			if (Ar.IsLoading())
			{
				if (Info & 0x80)	*(BITFIELD*)Value |= Bool->BitMask;
				else			*(BITFIELD*)Value &= ~Bool->BitMask;
			}
		}
		else
		{
			Property->SerializeItem(Ar, Value);
		}
	}*/
};

class TaggedObject
{
public:
	void SerializeTaggedProperties(FArchive& Ar)
	{
		if (Ar.IsLoading())
		{
			INT NumProps = 0;
			while (1)
			{
				FPropertyTag Tag;
				Ar << Tag;
				if (Tag.Name == NAME_None)
					break;
				debugfSlow(TEXT("GotProperty %ls skip %i bytes!"), *Tag.Name, Tag.Size);
				Ar.Seek(Ar.Tell() + Tag.Size);
				++NumProps;
			}
			debugfSlow(TEXT("Serialized %i properties"), NumProps);
		}
	}
};

class DukeMesh : public TaggedObject
{
public:
	FBox BoundingBox;
	FSphere BoundingSphere;
	FString ConfigName; // file name of project configuration

	void Serialize(FArchive& Ar)
	{
		SerializeTaggedProperties(Ar);
		Ar << BoundingBox << BoundingSphere;
		Ar << ConfigName;
	}
};

class LinkerLoad : public FArchive
{
public:
	FPackageFileSummary Summary;
	FArchive* FileAr;

	TArray<FName>			NameMap;			// Maps file name indices to name table indices.
	TArray<FObjectImport>	ImportMap;			// Maps file object indices >=0 to external object names.
	TArray<FObjectExport>	ExportMap;			// Maps file object indices >=0 to external object names.

	LinkerLoad(FArchive* Ar, const TCHAR* BasePath)
		: FileAr(Ar)
	{
		ArVer = PACKAGE_FILE_VERSION;
		ArIsLoading = ArIsPersistent = 1;

		*Ar << Summary;
		ArVer = Summary.FileVersion;
		ArLicenseeVer = Summary.LicenseeVersion;

		if (Summary.Tag != PACKAGE_FILE_TAG)
		{
			warnf(TEXT("Package had invalid tag (%i vs %i)!"), Summary.Tag, PACKAGE_FILE_TAG);
			return;
		}

		// Slack everything according to summary.
		ImportMap.Empty(Summary.ImportCount);
		ExportMap.Empty(Summary.ExportCount);
		NameMap.Empty(Summary.NameCount);

		// Load and map names.
		if (Summary.NameCount > 0)
		{
			Seek(Summary.NameOffset);

			for (INT i = 0; i < Summary.NameCount; i++)
			{
				// Read the name entry from the file.	
				FNameEntry NameEntry;

				appMemset(&NameEntry, 0, sizeof(NameEntry));		// JEP

				*this << NameEntry;

				// Add it to the name table if it's needed in this context.				
				NameMap.AddItem(FName(NameEntry.Name));
			}
		}

		// Load import map.
		if (Summary.ImportCount > 0)
		{
			Seek(Summary.ImportOffset);
			for (INT i = 0; i < Summary.ImportCount; i++)
				*this << *new(ImportMap)FObjectImport;
		}

		// Load export map.
		if (Summary.ExportCount > 0)
		{
			Seek(Summary.ExportOffset);
			for (INT i = 0; i < Summary.ExportCount; i++)
				*this << *new(ExportMap)FObjectExport;

			FName NAME_DukeMesh(TEXT("DukeMesh"));
			DukeMesh DM;
			DnfMesh Df;
			FStringOutputDevice StrExec;
			INT num = 0;
			for (INT i = 0; i < Summary.ExportCount; i++)
			{
				debugfSlow(TEXT("Export[%i] %ls Class %ls"), i, *ExportMap(i).ObjectName, *GetExportClassName(ExportMap(i).ClassIndex));
				if (GetExportClassName(ExportMap(i).ClassIndex) == NAME_DukeMesh)
				{
					FileAr->Seek(ExportMap(i).SerialOffset);
					DM.Serialize(*this);
					FString FileName = FString(BasePath) + DM.ConfigName;
					warnf(TEXT("============ Load Mesh %ls at %ls ============="), *ExportMap(i).ObjectName, *FileName);
					if (Df.LoadMesh(*FileName))
					{
						FString BasePath = appBaseDir();
						if (!num)
						{
							FString ModelDir = BasePath + TEXT("Models");
							GFileManager->MakeDirectory(*ModelDir);
						}
						if (Df.IsVertexMesh())
						{
							FString DDName = BasePath + TEXT("Models\\") + *ExportMap(i).ObjectName + TEXT("_d.3d");
							FArchive* FW = GFileManager->CreateFileWriter(*DDName);
							if (FW)
							{
								Df.Export3DD(*FW);
								delete FW;
							}
							DDName = BasePath + TEXT("Models\\") + *ExportMap(i).ObjectName + TEXT("_a.3d");
							FW = GFileManager->CreateFileWriter(*DDName);
							if (FW)
							{
								Df.Export3DA(*FW);
								delete FW;
							}
						}
						else
						{
							FString PskName = BasePath + TEXT("Models\\") + *ExportMap(i).ObjectName + TEXT(".psk");
							FArchive* FW = GFileManager->CreateFileWriter(*PskName);
							if (FW)
							{
								Df.ExportPSK(*FW);
								delete FW;
							}
							if (Df.IsAnimatedMesh())
							{
								PskName = BasePath + TEXT("Models\\") + *ExportMap(i).ObjectName + TEXT(".psa");
								FW = GFileManager->CreateFileWriter(*PskName);
								if (FW)
								{
									Df.ExportPSA(*FW);
									delete FW;
								}
							}
						}
						Df.ExportMeshInfo(*ExportMap(i).ObjectName, StrExec);
						++num;
					}
				}
			}
			if (num)
			{
				FString Path = FString(appBaseDir()) + TEXT("Imports.txt");
				appSaveStringToFile(StrExec, *Path);
			}
		}
	}

	inline FName GetExportClassName(INT Index)
	{
		if (Index < 0)
			return ImportMap(-Index - 1).ObjectName;
		else if (Index > 0)
			return ExportMap(Index - 1).ObjectName;
		else return NAME_Class;
	}

	void Serialize(void* V, INT Length)
	{
		FileAr->Serialize(V, Length);
	}
	INT Tell()
	{
		return FileAr->Tell();
	}
	INT TotalSize()
	{
		return FileAr->TotalSize();
	}
	UBOOL AtEnd()
	{
		return FileAr->AtEnd();
	}
	void Seek(INT InPos)
	{
		FileAr->Seek(InPos);
	}

	FArchive& operator<<(class UObject*& Object)
	{
		INT Index;
		*FileAr << AR_INDEX(Index);
		Object = NULL;

		return *this;
	}
	FArchive& operator<<(FName& Name)
	{
		NAME_INDEX NameIndex;
		*FileAr << AR_INDEX(NameIndex);

		if (!NameMap.IsValidIndex(NameIndex))
			appErrorf(TEXT("Bad name index %i/%i"), NameIndex, NameMap.Num());
		Name = NameMap(NameIndex);

		return *this;
	}
};
