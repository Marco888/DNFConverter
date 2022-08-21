#pragma once

typedef unsigned long NDword;
typedef unsigned short NWord;
typedef signed short NSWord;

#define CPJ_HDR_RIFF_MAGIC		"RIFF"
#define CPJ_HDR_FORM_MAGIC		"CPJB"

#define KRN_MEMCAST(xtype,xvar)		(*((xtype*)&(xvar)))
#define KRN_FOURCC(xstr)			(*((NDword*)xstr))

#define CPJVECTOR FVector
#define CPJQUAT FQuat

#if DEBUG_SINGLE_MESH
#define EDIT_FILE_TEXT 0
#endif
#if EDIT_FILE_TEXT
static FArchive* OutFile = NULL;
#endif

static const FQuat QBaseRot(0.5f, 0.5f, 0.5f, 0.5f);
static const FCoords CoordsBaseRot(FCoords::UnitCoords * FRotator(0, 16384, -16384));

inline void Q_AxisAngle(FQuat& Q, const FVector& inAxis, float inAngle) // construct in axis/angle form (named constructor)
{
	// v = -inAxis; v.Normalize(); v *= (float)sin(inAngle*0.5f); s = (float)cos(inAngle*0.5f);
	FVector V(inAxis);
	V = (-V).SafeNormal() * appSin(inAngle * 0.5f);
	Q.X = V.X;
	Q.Y = V.Y;
	Q.Z = V.Z;
	Q.W = appCos(inAngle * 0.5f);
}

struct VAxes3
{
	FVector vX, vY, vZ; // local normalized X, Y, and Z axes

	VAxes3() : vX(1, 0, 0), vY(0, 1, 0), vZ(0, 0, 1) {}
	VAxes3(const VAxes3& inF) { vX = inF.vX; vY = inF.vY; vZ = inF.vZ; }
	VAxes3(const FVector& inX, const FVector& inY, const FVector& inZ) { vX = inX; vY = inY; vZ = inZ; }
	VAxes3(const FVector& inAxis, float inAngle)
	{
		FQuat q;
		Q_AxisAngle(q, inAxis, inAngle);
		//q.AxisAngle(inAxis, inAngle);
		*this = q;
	}
	VAxes3(const FQuat& inQ)
	{
		float x(inQ.X), y(inQ.Y), z(inQ.Z), w(inQ.W);
		float x2(x * 2.0f), y2(y * 2.0f), z2(z * 2.0f), w2(w * 2.0f);
		float xx2(x * x2), yy2(y * y2), zz2(z * z2), ww2(w * w2);
		float xy2(x * y2), xz2(x * z2), xw2(x * w2), yz2(y * z2), yw2(y * w2), zw2(z * w2);

		vX = FVector(1.0f - (yy2 + zz2), xy2 + zw2, xz2 - yw2);
		vY = FVector(xy2 - zw2, 1.0f - (xx2 + zz2), yz2 + xw2);
		vZ = FVector(xz2 + yw2, yz2 - xw2, 1.0f - (xx2 + yy2));
	}
	VAxes3(const FRotator& inE)
	{
		FQuat q;
		*this = VAxes3();
		Q_AxisAngle(q, FVector(0, 0, 1), FLOAT(inE.Roll) * PI / 32768.f);
		//q.AxisAngle(FVector(0, 0, 1), inE.Roll);
		*this >>= q;
		Q_AxisAngle(q, FVector(1, 0, 0), FLOAT(inE.Pitch) * PI / 32768.f);
		//q.AxisAngle(FVector(1, 0, 0), inE.Pitch);
		*this >>= q;
		Q_AxisAngle(q, FVector(0, 1, 0), FLOAT(inE.Yaw) * PI / 32768.f);
		//q.AxisAngle(FVector(0, 1, 0), inE.Yaw);
		*this >>= q;
	}
	VAxes3(const FVector& inV)
	{
		vZ = inV; vZ.Normalize();
		vY = ~inV; vY.Normalize();
		vX = vY ^ vZ; vX.Normalize();
	}

	inline VAxes3& operator = (const VAxes3& inF) { vX = inF.vX; vY = inF.vY; vZ = inF.vZ; return(*this); }
	inline VAxes3& operator >>= (const VAxes3& inF) { *this = *this >> inF; return(*this); }
	inline VAxes3& operator <<= (const VAxes3& inF) { *this = *this << inF; return(*this); }

	inline friend VAxes3 operator ~ (const VAxes3& inF) { return(VAxes3() >> inF); } // inverse (transpose)
	inline friend VAxes3 operator & (const VAxes3& inF1, const VAxes3& inF2) { return(~inF2 << inF1); } // delta frame, frame1 >> result == frame2
	inline friend FVector operator >> (const FVector& inV, const VAxes3& inF) // world vector -> frame vector
	{
		return(FVector(
			(inV | inF.vX),
			(inV | inF.vY),
			(inV | inF.vZ)
		));
	}
	inline friend FVector operator << (const FVector& inV, const VAxes3& inF) // frame vector -> world vector
	{
		return(FVector(
			(inV.X * inF.vX.X + inV.Y * inF.vY.X + inV.Z * inF.vZ.X),
			(inV.X * inF.vX.Y + inV.Y * inF.vY.Y + inV.Z * inF.vZ.Y),
			(inV.X * inF.vX.Z + inV.Y * inF.vY.Z + inV.Z * inF.vZ.Z)
		));
	}
	inline friend VAxes3 operator >> (const VAxes3& inF1, const VAxes3& inF2) // world frame1 -> frame2-relative frame1
	{
		return(VAxes3(inF1.vX >> inF2, inF1.vY >> inF2, inF1.vZ >> inF2));
	}
	inline friend VAxes3 operator << (const VAxes3& inF1, const VAxes3& inF2) // frame2-relative frame1 -> world frame1
	{
		return(VAxes3(inF1.vX << inF2, inF1.vY << inF2, inF1.vZ << inF2));
	}

	inline FQuat GetQuat() const
	{
		// verify the axes are completely normalized
		FQuat Result;
		VAxes3 adjF(*this);
		adjF.vX.Normalize();
		adjF.vY.Normalize();
		adjF.vZ.Normalize();

		static int rot1[3] = { 1, 2, 0 };
		const float* c[3] = { &adjF.vX.X, &adjF.vY.X, &adjF.vZ.X };
		int i, j, k;
		float d, sq, q[4];
		d = c[0][0] + c[1][1] + c[2][2];
		if (d > 0.0)
		{
			sq = appSqrt(d + 1.0f);
			Result.W = sq * 0.5f;
			sq = 0.5f / sq;
			Result.X = (c[1][2] - c[2][1]) * sq;
			Result.Y = (c[2][0] - c[0][2]) * sq;
			Result.Z = (c[0][1] - c[1][0]) * sq;
		}
		else
		{
			i = 0;
			if (c[1][1] > c[0][0]) i = 1;
			if (c[2][2] > c[i][i]) i = 2;
			j = rot1[i];
			k = rot1[j];
			sq = appSqrt((c[i][i] - (c[j][j] + c[k][k])) + 1.0f);
			q[i] = sq * 0.5f;
			if (sq != 0.0f)
				sq = 0.5f / sq;
			Result.W = (c[j][k] - c[k][j]) * sq;
			q[j] = (c[i][j] + c[j][i]) * sq;
			q[k] = (c[i][k] + c[k][i]) * sq;
			Result.X = q[0];
			Result.Y = q[1];
			Result.Z = q[2];
		}
		return Result;
	}
	inline FQuat GetUEQuat() const
	{
		FQuat Result;

		// Trace.
		FLOAT Trace = vX.X + vY.Y + vZ.Z + 1.0f;
		// Calculate directly for positive trace.
		if (Trace > 0.f)
		{
			FLOAT S = 0.5f / appSqrt(Trace);
			Result.W = 0.25f / S;
			Result.X = (vZ.Y - vY.Z) * S;
			Result.Y = (vX.Z - vZ.X) * S;
			Result.Z = (vY.X - vX.Y) * S;
		}
		// Or determine the major diagonal element.
		else if ((vX.X > vY.Y) && (vX.X > vZ.Z))
		{
			FLOAT SZ = 0.5f / appSqrt(1.0f + vX.X - vY.Y - vZ.Z);
			Result.X = 0.5f * SZ;
			Result.Y = (vX.Y + vY.X) * SZ;
			Result.Z = (vX.Z + vZ.X) * SZ;
			Result.W = (vY.Z + vZ.Y) * SZ;
		}
		else if (vY.Y > vZ.Z)
		{
			FLOAT SZ = 0.5f / appSqrt(1.0f + vY.Y - vX.X - vZ.Z);
			Result.X = (vX.Y + vY.X) * SZ;
			Result.Y = 0.5f * SZ;
			Result.Z = (vY.Z + vZ.Y) * SZ;
			Result.W = (vX.Z + vZ.X) * SZ;
		}
		else
		{
			FLOAT SZ = 0.5f / appSqrt(1.0f + vZ.Z - vX.X - vY.Y);
			Result.X = (vX.Z + vZ.X) * SZ;
			Result.Y = (vY.Z + vZ.Y) * SZ;
			Result.Z = 0.5f * SZ;
			Result.W = (vX.Y + vY.X) * SZ;
		}
		return Result;
	}
};

// Byte describing effects for a mesh triangle.
enum EJSMeshTriType
{
	// Triangle types. Mutually exclusive.
	MTT_Normal = 0,	// Normal one-sided.
	MTT_NormalTwoSided = 1,    // Normal but two-sided.
	MTT_Translucent = 2,	// Translucent two-sided.
	MTT_Masked = 3,	// Masked two-sided.
	MTT_Modulate = 4,	// Modulation blended two-sided.
	MTT_AlphaBlend = 5,	// Alphablend two-sided.
	MTT_Placeholder = 8,	// Placeholder triangle for positioning weapon. Invisible.
	// Bit flags.
	MTT_Unlit = 16,	// Full brightness, no lighting.
	MTT_Flat = 32,	// Flat surface, don't do bMeshCurvy thing.
	MTT_Environment = 64,	// Environment mapped.
	MTT_NoSmooth = 128,	// No bilinear filtering on this poly's texture.
};

struct FUVMap
{
	FLOAT U, V;

	inline void Set(FLOAT InU, FLOAT InV)
	{
		U = InU;
		V = InV;
	}
};

static TCHAR MagicResult[5] = { 0,0,0,0,0 };
inline const TCHAR* DecodeMagic(NDword mg)
{
	MagicResult[0] = (mg & 0xFF);
	MagicResult[1] = ((mg >> 8) & 0xFF);
	MagicResult[2] = ((mg >> 16) & 0xFF);
	MagicResult[3] = ((mg >> 24) & 0xFF);
	return MagicResult;
}

struct SCpjFileHeader
{
	NDword riffMagic; // CPJ_HDR_RIFF_MAGIC
	NDword lenFile; // length of file following this value
	NDword formMagic; // CPJ_HDR_FORM_MAGIC

	friend FArchive& operator<<(FArchive& Ar, SCpjFileHeader& H)
	{
		return Ar << H.riffMagic << H.lenFile << H.formMagic;
	}
};
struct SCpjChunkHeader
{
	NDword magic; // chunk-specific magic marker
	NDword lenFile; // length of chunk following this value
	NDword version; // chunk-specific format version
	NDword timeStamp; // time stamp of chunk creation
	NDword ofsName; // offset of chunk name string from start of chunk
						   // If this value is zero, the chunk is nameless
};
struct SMacSection
{
	NDword ofsName; // offset of section name string in data block
	NDword numCommands; // number of command strings in section
	NDword firstCommand; // first command string index
};
struct CCpjMacSection
{
	FString name;
	TArray<FString> commands;
};
struct SMacFile
{
	SCpjChunkHeader header; // header information

	// sections (array of SMacSection)
	NDword numSections; // number of sections
	NDword ofsSections; // offset of sections in data block

	// command strings (array of unsigned long, offsets into data block)
	NDword numCommands; // number of commands
	NDword ofsCommands; // offset of command strings in data block

	// data block
	BYTE dataBlock[256]; // variable sized data block
};

enum
{
	GEOVF_LODLOCK = 0x00000001 // vertex is locked during LOD processing
};

struct SGeoVert
{
	unsigned char flags; // GEOVF_ vertex flags
	unsigned char groupIndex; // group index for vertex frame compression
	unsigned short reserved; // reserved for future use, must be zero
	unsigned short numEdgeLinks; // number of edges linked to this vertex
	unsigned short numTriLinks; // number of triangles linked to this vertex
	unsigned long firstEdgeLink; // first edge index in object link array
	unsigned long firstTriLink; // first triangle index in object link array
	CPJVECTOR refPosition; // reference position of vertex
};
struct SGeoEdge
{
	unsigned short headVertex; // vertex list index of edge's head vertex
	unsigned short tailVertex; // vertex list index of edge's tail vertex
	unsigned short invertedEdge; // edge list index of inverted mirror edge
	unsigned short numTriLinks; // number of triangles linked to this edge
	unsigned long firstTriLink; // first triangle index in object link array
};
struct SGeoTri
{
	unsigned short edgeRing[3]; // edge list indices used by triangle, whose
								// tail vertices are V0, V1, and V2, in order
	unsigned short reserved; // reserved for future use, must be zero
};
struct SGeoMount
{
	unsigned long ofsName; // offset of mount point name string in data block
	unsigned long triIndex; // triangle index of mount base
	CPJVECTOR triBarys; // barycentric coordinates of mount origin
	CPJVECTOR baseScale; // base transform scaling
	CPJQUAT baseRotate; // base transform rotation quaternion
	CPJVECTOR baseTranslate; // base transform translation

	// A mount's runtime transform is calculated as the base transform
	// shifted out of a second transform determined by the triangle on the
	// fly.  This transform has a unit scale, a translation of the point on
	// the triangle described by the given barycentric coordinates, and a
	// rotation described by a specific axial frame.  This axial frame has
	// a Y axis that is the normal of the triangle, a Z axis that is the
	// normalized vector from the mount origin point to the triangle's V0,
	// and a X axis that is the cross product of these Y and Z axes.
};
struct SGeoFile
{
	SCpjChunkHeader header; // header information

	// vertices (array of SGeoVert)
	NDword numVertices; // number of vertices
	NDword ofsVertices; // offset of vertices in data block

	// edges (array of SGeoEdge)
	NDword numEdges; // number of edges
	NDword ofsEdges; // offset of edges in data block

	// triangles (array of SGeoTri)
	NDword numTris; // number of triangles
	NDword ofsTris; // offset of triangles in data block

	// mount points (array of SGeoMount)
	NDword numMounts; // number of mounts
	NDword ofsMounts; // offset of mounts in data block

	// object links (array of unsigned short)
	NDword numObjLinks; // number of object links
	NDword ofsObjLinks; // number of object links in data

	// data block
	BYTE dataBlock[256]; // variable sized data block
};

struct VCoords3
{
	VAxes3 mr;
	FMatrix mtx;
	FQuat r; // axial frame (rotation)
	FVector t; // local origin (translation)
	FVector s; // axis scale values (scale)

	VCoords3()
		: mr(), mtx(FMatrix::Identity), r(FQuat::Identity), t(0, 0, 0), s(1, 1, 1)
	{}
	VCoords3(const VCoords3& inC)
		: mr(inC.mr), mtx(inC.mtx), r(inC.r), t(inC.t), s(inC.s)
	{}
	VCoords3(const FQuat& inRotate, const FVector& inTranslate = FVector(0, 0, 0), const FVector& inScale = FVector(1, 1, 1))
		: mr(inRotate), mtx(FVector(0,0,0), inRotate), r(inRotate), t(inTranslate), s(inScale)
	{}
	VCoords3(const VAxes3& inRotate, const FVector& inTranslate = FVector(0, 0, 0), const FVector& inScale = FVector(1, 1, 1))
		: mr(inRotate), mtx(FVector(0, 0, 0), inRotate.GetQuat()), r(inRotate.GetQuat()), t(inTranslate), s(inScale)
	{}

	inline void UpdateQuat(const FQuat& NewQuat)
	{
		r = NewQuat;
		mr = NewQuat;
		mtx = FMatrix(FVector(0, 0, 0), NewQuat);
	}

	inline friend FVector operator >> (const FVector& inV, const VCoords3& inC) // world position -> coords position
	{
		return (((inV - inC.t) >> inC.mr) / inC.s);
	}
	inline friend FVector operator << (const FVector& inV, const VCoords3& inC) // coords position -> world position
	{
		return (((inV * inC.s) << inC.mr) + inC.t);
	}
	inline friend VCoords3 operator >> (const VCoords3& inC1, const VCoords3& inC2) // world coords1 -> coords2-relative coords1
	{
		return(VCoords3(inC1.mr >> inC2.mr, inC1.t >> inC2, inC1.s / inC2.s));
	}
	inline friend VCoords3 operator << (const VCoords3& inC1, const VCoords3& inC2) // coords2-relative coords1 -> world coords1
	{
		return(VCoords3(inC1.mr << inC2.mr, inC1.t << inC2, inC1.s * inC2.s));
	}
	inline VCoords3& operator >>= (const VCoords3& inC) { *this = *this >> inC; return(*this); }
	inline VCoords3& operator <<= (const VCoords3& inC) { *this = *this << inC; return(*this); }
};

struct CCpjGeoVert
{
	BYTE flags;
	BYTE groupIndex;
	FVector refPosition;
	INT iVertex;
	BYTE RefCount;
	TArray<struct CCpjGeoEdge*> edgeLinks;
	TArray<struct CCpjGeoTri*> triLinks;
};
struct CCpjGeoEdge
{
	CCpjGeoVert* headVertex;
	CCpjGeoVert* tailVertex;
	CCpjGeoEdge* invertedEdge;
	TArray<struct CCpjGeoTri*> triLinks;

	CCpjGeoEdge() { headVertex = tailVertex = NULL; invertedEdge = NULL; }
};
struct CCpjGeoTri
{
	CCpjGeoEdge* edgeRing[3];
};
struct CCpjGeoMount
{
	TCHAR name[64];
	NDword triIndex;
	FVector triBarys;
	VCoords3 baseCoords;
};

struct SSrfTex
{
	unsigned long ofsName; // offset of texture name string in data block
	unsigned long ofsRefName; // offset of optional reference name in block
};
enum
{
	SRFTF_INACTIVE = 0x00000001, // triangle is not active
	SRFTF_HIDDEN = 0x00000002, // present but invisible
	SRFTF_VNIGNORE = 0x00000004, // ignored in vertex normal calculations
	SRFTF_TRANSPARENT = 0x00000008, // transparent rendering is enabled
	SRFTF_UNLIT = 0x00000020, // not affected by dynamic lighting
	SRFTF_TWOSIDED = 0x00000040, // visible from both sides
	SRFTF_MASKING = 0x00000080, // color key masking is active
	SRFTF_MODULATED = 0x00000100, // modulated rendering is enabled
	SRFTF_ENVMAP = 0x00000200, // environment mapped
	SRFTF_NONCOLLIDE = 0x00000400, // traceray won't collide with this surface
	SRFTF_TEXBLEND = 0x00000800,
	SRFTF_ZLATER = 0x00001000,
	SRFTF_RESERVED = 0x00010000
};
enum ESrfGlaze
{
	SRFGLAZE_NONE = 0,	// no glaze pass
	SRFGLAZE_SPECULAR	// fake specular glaze
};
struct SSrfTri
{
	unsigned short uvIndex[3]; // UV texture coordinate indices used
	unsigned char texIndex; // surface texture index
	unsigned char reserved; // reserved for future use, must be zero
	unsigned long flags; // SRFTF_ triangle flags
	unsigned char smoothGroup; // light smoothing group
	unsigned char alphaLevel; // transparent/modulated alpha level
	unsigned char glazeTexIndex; // second-pass glaze texture index if used
	unsigned char glazeFunc; // ESrfGlaze second-pass glaze function
};
struct SSrfUV
{
	float u; // texture U coordinate
	float v; // texture V coordinate
};
struct SSrfFile
{
	SCpjChunkHeader header; // header information

	// textures (array of SSrfTex)
	unsigned long numTextures; // number of textures
	unsigned long ofsTextures; // offset of textures in data block

	// triangles (array of SSrfTri)
	unsigned long numTris; // number of triangles
	unsigned long ofsTris; // offset of triangles in data block

	// UV texture coordinates (array of SSrfUV)
	unsigned long numUV; // number of UV texture coordinates
	unsigned long ofsUV; // offset of UV texture coordinates in data block

	// data block
	BYTE dataBlock[128]; // variable sized data block
};

struct CCpjSrfTex
{
	TCHAR name[128];
	TCHAR refName[128];
};
struct CCpjSrfTri
{
	NWord uvIndex[3];
	BYTE texIndex;
	NDword flags;
	BYTE smoothGroup;
	BYTE alphaLevel;
	BYTE glazeTexIndex;
	BYTE glazeFunc;

	CCpjSrfTri()
	{
		uvIndex[0] = uvIndex[1] = uvIndex[2] = 0;
		texIndex = 0;
		flags = 0x00000001; // SRFTF_INACTIVE;
		smoothGroup = alphaLevel = glazeTexIndex = glazeFunc = 0;
	}
	inline BYTE GetMTFlags() const
	{
		BYTE Result = MTT_Normal;
		if (flags & SRFTF_TRANSPARENT)
			Result = MTT_Translucent;
		else if (flags & SRFTF_MODULATED)
			Result = MTT_Modulate;
		else if (flags & SRFTF_MASKING)
			Result = MTT_Masked;
		else if (flags & SRFTF_TWOSIDED)
			Result = MTT_NormalTwoSided;
		if (flags & SRFTF_ENVMAP)
			Result |= MTT_Environment;
		if (flags & SRFTF_UNLIT)
			Result |= MTT_Unlit;
		return Result;
	}
};

struct SSklBone
{
	unsigned long ofsName; // offset of bone name string in data block
	unsigned long parentIndex; // parent bone index, -1 if none
	CPJVECTOR baseScale; // base transform scaling
	CPJQUAT baseRotate; // base transform rotation quaternion
	CPJVECTOR baseTranslate; // base transform translation
	float length; // length of the bone, used for rotation adjustments
};
struct SSklVert
{
	unsigned short numWeights; // number of skeletal weights
	unsigned short firstWeight; // first index in skeletal weights
};
struct SSklWeight
{
	unsigned long boneIndex; // index of bone used by weight
	float weightFactor; // weighting factor, [0.0-1.0]
	CPJVECTOR offsetPos; // offset position vector
};
struct SSklMount
{
	unsigned long ofsName; // offset of mount point name string in data block
	unsigned long boneIndex; // bone index of mount base, -1 if origin
	CPJVECTOR baseScale; // base transform scaling
	CPJQUAT baseRotate; // base transform rotation quaternion
	CPJVECTOR baseTranslate; // base transform translation
};
struct SSklFile
{
	SCpjChunkHeader header; // header information

	// skeletal bones (array of SSklBone)
	unsigned long numBones; // number of skeletal bones
	unsigned long ofsBones; // offset of skeletal bones in data block

	// skeletal vertices (array of SSklVert)
	unsigned long numVerts; // number of skeletal vertices
	unsigned long ofsVerts; // offset of skeletal vertices in data block

	// skeletal weights (array of SSklWeight)
	unsigned long numWeights; // number of skeletal weights
	unsigned long ofsWeights; // offset of skeletal weights in data block

	// bone mounts (array of SSklMount)
	unsigned long numMounts; // number of bone mounts
	unsigned long ofsMounts; // offset of bone mounts in data block

	// data block
	BYTE dataBlock[128]; // variable sized data block
};

struct CCpjSklBone
{
	FString name;
	CCpjSklBone* parentBone;
	VCoords3 baseCoords;
	FLOAT length;
	INT Index, ParentIndex;

	FQuat FinalQuat;
	FVector FinalPose;

	FQuat GetParentQuat() const
	{
		FQuat Result = baseCoords.r;
		if (parentBone)
			Result = Result * parentBone->GetParentQuat();
		return Result;
	}
	VAxes3 GetParentAxes() const
	{
		if (parentBone)
		{
			VAxes3 Parent = parentBone->GetParentAxes();
			return baseCoords.mr << Parent;
		}
		return baseCoords.mr;
	}
	FCoords GetParentFCoords() const
	{
		FCoords Result(baseCoords.t, baseCoords.r);
		if (parentBone)
			Result = Result.ApplyPivot(parentBone->GetParentFCoords());
		return Result;
	}
	FVector GetParentPose() const
	{
		FVector Result = baseCoords.t;
		if (parentBone)
			Result += parentBone->GetParentPose();
		return Result;
	}
	VCoords3 GetParentCoords() const
	{
		if (parentBone)
			return baseCoords << parentBone->GetParentCoords();
		return baseCoords;
	}
};
struct CCpjSklWeight
{
	INT BoneIndex;
	FLOAT factor;
	FVector offsetPos;
};
struct CCpjSklVert
{
	TArray<CCpjSklWeight> weights;
};
struct CCpjSklMount
{
	FString name;
	CCpjSklBone* bone;
	VCoords3 baseCoords;
};

struct SFrmBytePos
{
	unsigned char group; // compression group number
	unsigned char pos[3]; // byte position
};
struct SFrmGroup
{
	CPJVECTOR byteScale; // scale byte positions by this
	CPJVECTOR byteTranslate; // add to positions after scale
};
struct SFrmFrame
{
	unsigned long ofsFrameName; // offset of frame name in data block

	// frame bounding box
	CPJVECTOR bbMin; // bounding box minimum
	CPJVECTOR bbMax; // bounding box maximum

	// byte compression groups (array of SFrmGroup)
	unsigned long numGroups; // number of byte compression groups, zero means
							 // frame is uncompressed, otherwise compressed
	unsigned long ofsGroups; // offset of groups in data block, if compressed

	// vertex positions
	// array of CPJVECTOR if uncompressed
	// array of SFrmBytePos if compressed
	unsigned long numVerts; // number of vertex positions
	unsigned long ofsVerts; // offset of vertex positions in data block
};
struct SFrmFile
{
	SCpjChunkHeader header; // header information

	// bounding box of all frames
	CPJVECTOR bbMin; // bounding box minimum
	CPJVECTOR bbMax; // bounding box maximum

	// vertex frames (array of SFrmFrame)
	unsigned long numFrames; // number of vertex frames
	unsigned long ofsFrames; // offset of vertex frames in data block

	// data block
	BYTE dataBlock[128]; // variable sized data block
};

struct CCpjFrmBytePos
{
	BYTE group;
	BYTE pos[3];
};
struct CCpjFrmGroup
{
	FVector scale;
	FVector translate;
};
struct CCpjFrmFrame
{
	FString m_Name;
	UBOOL m_isCompressed;
	TArray<CCpjFrmGroup> m_Groups;
	TArray<CCpjFrmBytePos> m_BytePos;
	TArray<FVector> m_PurePos;
	FBox m_Bounds;
};

struct SSeqBoneInfo
{
	unsigned long ofsName; // offset of bone name string in data block
	float srcLength; // source skeleton bone length
};
struct SSeqBoneTranslate
{
	unsigned short boneIndex; // bone info index
	unsigned short reserved; // must be zero
	CPJVECTOR translate; // translation vector
};
struct SSeqBoneRotate
{
	unsigned short boneIndex; // bone info index
	signed short roll; // roll about Z axis in 64k degrees, followed by...
	signed short pitch; // pitch about X axis in 64k degrees, followed by...
	signed short yaw; // yaw about Y axis in 64k degrees
};
struct SSeqBoneScale
{
	unsigned short boneIndex; // bone info index
	unsigned short reserved; // must be zero
	CPJVECTOR scale; // component scaling values
};

#define CPJ_SEQEV_FOURCC(a,b,c,d) ((a)+((b)<<8)+((c)<<16)+((d)<<24))

enum ESeqEvent
{
	SEQEV_INVALID = 0,
	// string is a marker, not an actual event
	SEQEV_MARKER = CPJ_SEQEV_FOURCC('M', 'R', 'K', 'R'),
	// fire a trigger notification string, application specific use
	SEQEV_TRIGGER = CPJ_SEQEV_FOURCC('T', 'R', 'I', 'G'),
	// send a MAC chunk command to the backing actor
	SEQEV_ACTORCMD = CPJ_SEQEV_FOURCC('A', 'C', 'M', 'D'),
	// triangle flag alteration, string is a character string of hex digits,
	// one byte per triangle (length should match surface chunk triangle
	// count), for 4 possible flags.  Hex digits A through F must be uppercase.
	// Bit 0: Triangle is hidden
	// Bit 1-3: Currently unused
	SEQEV_TRIFLAGS = CPJ_SEQEV_FOURCC('T', 'F', 'L', 'G'),
};
struct SSeqFrame
{
	unsigned char reserved; // reserved for future use, must be zero
	unsigned char numBoneTranslate; // number of bone translations
	unsigned char numBoneRotate; // number of bone rotations
	unsigned char numBoneScale; // number of bone scalings
	unsigned long firstBoneTranslate; // first bone translation index
	unsigned long firstBoneRotate; // first bone rotation index
	unsigned long firstBoneScale; // first bone scaling index
	unsigned long ofsVertFrameName; // offset of vertex frame name in data
									// block or -1 if no vertex frame is used
};
struct SSeqEvent
{
	unsigned long eventType; // ESeqEvent event type
	float time; // sequence time of event, from zero to one
	unsigned long ofsParam; // offset of parameter string in data block,
							// or -1 if string not used
};
struct SSeqFile
{
	SCpjChunkHeader header; // header information

	// global sequence information
	float playRate; // sequence play rate in frames per second

	// sequence frames
	unsigned long numFrames; // number of sequence frames
	unsigned long ofsFrames; // offset of sequence frames in data

	// sequence events, in chronological order
	unsigned long numEvents; // number of events
	unsigned long ofsEvents; // offset of events in data

	// bone info (array of SSeqBoneInfo)
	unsigned long numBoneInfo; // number of bone info
	unsigned long ofsBoneInfo; // offset of bone info in data block

	// bone translations (array of SSeqBoneTranslate)
	unsigned long numBoneTranslate; // number of bone translations
	unsigned long ofsBoneTranslate; // offset of bone translations in data

	// bone rotations (array of SSeqBoneRotate)
	unsigned long numBoneRotate; // number of bone rotations
	unsigned long ofsBoneRotate; // offset of bone rotations in data

	// bone scalings (array of SSeqBoneScale)
	unsigned long numBoneScale; // number of bone scalings
	unsigned long ofsBoneScale; // offset of bone scalings in data

	// data block
	BYTE dataBlock[128]; // variable sized data block
};

struct CCpjSeqBoneInfo
{
	FString name;
	FLOAT srcLength;
	INT iRefBone; // Reference to skm_Bones array.
	INT iParent; // Reference to parent bone CCpjSeqBoneInfo index
};
struct CCpjSeqTranslate
{
public:
	NWord boneIndex;
	NWord reserved;
	FVector translate, FinalPose;
};
struct CCpjSeqRotate
{
public:
	INT boneIndex;
	FQuat quat,InvQuat;
	VAxes3 axes;
};
struct CCpjSeqScale
{
	NWord boneIndex;
	NWord reserved;
	FVector scale;
};
struct CCpjSeqFrame
{
	FString vertFrameName;
	TArray<CCpjSeqTranslate> translates;
	TArray<CCpjSeqRotate> rotates;
	TArray<CCpjSeqScale> scales;

	inline INT FindRot(INT RefBone) const
	{
		for (INT i = 0; i < rotates.Num(); ++i)
			if (rotates(i).boneIndex == (NWord)RefBone)
				return i;
		return INDEX_NONE;
	}
	FCoords GetParentFCoords(const CCpjSklBone& Bone) const
	{
		FCoords Result(translates(Bone.Index).translate, rotates(Bone.Index).quat);
		if (Bone.parentBone)
			Result = Result.ApplyPivot(GetParentFCoords(*Bone.parentBone));
		return Result;
	}
	VAxes3 GetParentAxes(const CCpjSklBone& Bone) const
	{
		if (Bone.parentBone)
		{
			VAxes3 Parent = GetParentAxes(*Bone.parentBone);
			return VAxes3(rotates(Bone.Index).quat) << Parent;
		}
		return VAxes3(rotates(Bone.Index).quat);
	}
	FQuat GetParentQuat(const CCpjSklBone& Bone) const
	{
		if (Bone.parentBone)
		{
			FQuat Parent = GetParentQuat(*Bone.parentBone);
			return Parent * rotates(Bone.Index).quat;
		}
		return rotates(Bone.Index).quat;
	}
};
struct CCpjSeqEvent
{
	NDword eventType;
	FLOAT time;
	FString paramString;
};

struct SLodTri
{
	unsigned long srfTriIndex; // original surface triangle index
	unsigned short vertIndex[3]; // relayed vertex indices used by triangle
	unsigned short uvIndex[3]; // surface UV indices used by triangle
};
struct SLodLevel
{
	float detail; // maximum detail value of this level, from zero to one
	unsigned long numTriangles; // number of triangles in level
	unsigned long numVertRelay; // number of vertices in level relay
	unsigned long firstTriangle; // first triangle in triangle list
	unsigned long firstVertRelay; // first index in vertex relay
};
struct SLodFile
{
	SCpjChunkHeader header; // header information

	// levels (array of SLodLevel)
	unsigned long numLevels; // number of levels
	unsigned long ofsLevels; // offset of levels in data block

	// triangles (array of SLodTri)
	unsigned long numTriangles; // number of triangles
	unsigned long ofsTriangles; // offset of triangles in data block

	// vertex relay (array of unsigned short)
	unsigned long numVertRelay; // number of vertices in relay
	unsigned long ofsVertRelay; // offset of vertex relay in data block

	// data block
	BYTE dataBlock[128]; // variable sized data block
};
struct CCpjLodTri
{
	NDword srfTriIndex;
	NWord vertIndex[3];
	NWord uvIndex[3];
};
struct CCpjLodLevel
{
	FLOAT detail;
	TArray<NWord> vertRelay;
	TArray<CCpjLodTri> triangles;
};

#if 1
inline FVector ToUECoords(const FVector& V)
{
	return FVector(V.Z, -V.X, V.Y);
}
inline FVector ToUEVector(const FVector& V)
{
	return FVector(V.Z, V.X, V.Y);
}
inline FQuat ToUEQuat(const FQuat& Q)
{
	return FQuat(Q.Z, Q.X, Q.Y, Q.W);
}
inline FQuat ToDnfQuat(const FQuat& Q)
{
	return FQuat(Q.Y, Q.Z, Q.X, Q.W);
}
#else
inline FVector ToUECoords(const FVector& V)
{
	return V;
}
inline FVector ToUEVector(const FVector& V)
{
	return V;
}
inline FQuat ToUEQuat(const FQuat& Q)
{
	return Q;
}
inline FQuat ToDnfQuat(const FQuat& Q)
{
	return Q;
}
#endif

inline FString ReadStringByte(BYTE* Data)
{
	if (!*Data)
		return FString();
	FString Result;
	TArray<TCHAR>& SA = Result.GetCharArray();
	while (*Data)
	{
		SA.AddItem(*Data);
		++Data;
	}
	SA.AddItem(0);
	return Result;
}
inline void StrByteCopy(TCHAR* Dest, const BYTE* Src, INT MaxLen)
{
	INT i = 0;
	--MaxLen;
	for (; (i < MaxLen && Src[i]); ++i)
		Dest[i] = (TCHAR)Src[i];
	Dest[i] = 0;
}

struct FAnimSequence
{
	FString AnimName;
	FLOAT m_Rate;
	TArray<CCpjSeqFrame> m_Frames;
	TArray<CCpjSeqEvent> m_Events;
	TArray<CCpjSeqBoneInfo> m_BoneInfo;

	inline INT FindBone(const TCHAR* BoneName) const
	{
		for (INT i = 0; i < m_BoneInfo.Num(); ++i)
			if (m_BoneInfo(i).name == BoneName)
				return i;
		return INDEX_NONE;
	}
};

struct VMaterial
{
	ANSICHAR            MaterialName[64];
	INT					TextureIndex;  // texture index ('multiskin index')
	DWORD				PolyFlags;     // ALL poly's with THIS material will have this flag.
	INT				    AuxMaterial;   // reserved: index into another material, eg. detailtexture/shininess/whatever.
	DWORD				AuxFlags;      // reserved: auxiliary flags 
	INT					LodBias;       // material-specific lod bias
	INT					LodStyle;      // material-specific lod style
};

inline void SwapBones(CCpjSklBone* B, const INT Num, const INT IndexA, const INT IndexB, TArray<CCpjSklVert>& Verts)
{
	appMemswap(&B[IndexA], &B[IndexB], sizeof(CCpjSklBone));
	B[IndexA].Index = IndexA;
	B[IndexB].Index = IndexB;

	// Update references.
	INT i;
	for (i = 0; i < Num; ++i)
	{
		if (B[i].ParentIndex == IndexA)
		{
			B[i].ParentIndex = IndexB;
			B[i].parentBone = &B[IndexB];
		}
		else if (B[i].ParentIndex == IndexB)
		{
			B[i].ParentIndex = IndexA;
			B[i].parentBone = &B[IndexA];
		}
	}
	INT j;
	for (i = (Verts.Num() - 1); i >= 0; --i)
	{
		CCpjSklWeight* w = &Verts(i).weights(0);
		for (j = (Verts(i).weights.Num() - 1); j >= 0; --j)
		{
			if (w[j].BoneIndex == IndexA)
				w[j].BoneIndex = IndexB;
			else if (w[j].BoneIndex == IndexB)
				w[j].BoneIndex = IndexA;
		}
	}
}

class DnfMesh
{
	FString CurFile;

	TArray<FAnimSequence> Animations; // Sequences
	TArray<CCpjMacSection> m_Sections; // MAC

	TArray<CCpjGeoVert> m_Verts; // Geometry
	TArray<CCpjGeoEdge> m_Edges; // Geometry
	TArray<CCpjGeoTri> m_Tris; // Geometry
	TArray<CCpjGeoMount> m_Mounts; // Geometry

	TArray<CCpjSrfTex> m_Textures; // Surfaces
	TArray<CCpjSrfTri> sm_Tris; // Surfaces
	TArray<FUVMap> m_UV; // Surfaces

	TArray<CCpjSklBone> skm_Bones; // Skeleton
	TArray<CCpjSklVert> skm_Verts; // Skeleton
	TArray<CCpjSklMount> skm_Mounts; // Skeleton

	TArray<CCpjLodLevel> m_Levels; // LOD

	FBox m_Bounds;
	TArray<CCpjFrmFrame> m_Frames; // Anim frames

	FVector VertOrigin, VertScaling;

	TArray<FString> ExportedTex;

	// Cmd data
	FString CMD_Author;
	FVector CMD_Offset, CMD_Scale;
	FRotator CMD_RotOffset;
	FBox CMD_Bounds;

	TArray<VMaterial> PSK_Materials;

	inline const CCpjFrmFrame* FindFrame(const TCHAR* FrameName) const
	{
		for (INT i = 0; i < m_Frames.Num(); ++i)
			if (m_Frames(i).m_Name == FrameName)
				return &m_Frames(i);
		return nullptr;
	}
	inline INT FindBone(const TCHAR* BoneName) const
	{
		for (INT i = 0; i < skm_Bones.Num(); ++i)
			if (skm_Bones(i).name == BoneName)
				return i;
		return INDEX_NONE;
	}
	inline void Clear()
	{
		Animations.Empty();
		m_Sections.Empty();
		m_Verts.Empty();
		m_Edges.Empty();
		m_Tris.Empty();
		m_Mounts.Empty();
		m_Textures.Empty();
		sm_Tris.Empty();
		m_UV.Empty();
		skm_Bones.Empty();
		skm_Verts.Empty();
		skm_Mounts.Empty();
		m_Levels.Empty();
		m_Frames.Empty();

		CMD_Author.Empty();
		CMD_Offset = FVector(0, 0, 0);
		CMD_Scale = FVector(1, 1, 1);
		CMD_RotOffset = FRotator(0, 0, 0);
		CMD_Bounds = FBox(FVector(-128.f, -128.f, -128.f), FVector(128.f, 128.f, 128.f));
	}
	inline void CalcBestScaling()
	{
		FBox TotalBounds(0);
		for (INT i = 0; i < m_Frames.Num(); ++i)
		{
			for (INT j = (m_Frames(i).m_PurePos.Num() - 1); j >= 0; --j)
				TotalBounds += m_Frames(i).m_PurePos(j);
		}
		TotalBounds.GetCenterAndExtents(VertOrigin, VertScaling);
		VertScaling = FVector(1023.f / VertScaling.X, 1023.f / VertScaling.Y, 511.f / VertScaling.Z);
	}
	inline FVector PreMulti(const FVector& V) const
	{
		return (V - VertOrigin) * VertScaling;
	}
	inline void TransformToUE()
	{
		INT i, j;
		for (i = 0; i < m_Sections.Num(); ++i)
		{
			const CCpjMacSection& C = m_Sections(i);
			if (!appStricmp(*C.name, TEXT("autoexec")))
			{
				for (j = 0; j < C.commands.Num(); ++j)
				{
					const TCHAR* Cmd = *C.commands(j);
					GLog->Log(NAME_Cmd, Cmd);
					if (ParseCommand(&Cmd, TEXT("SetAuthor")))
					{
						while (*Cmd == ' ')
							++Cmd;
						if (*Cmd == '\"')
							++Cmd;
						const TCHAR* End = Cmd;
						while (*End && End[1])
							++End;
						if (*End != '\"')
							++End;
						CMD_Author = FString(Cmd, End);
					}
					else if (ParseCommand(&Cmd, TEXT("SetOrigin")))
					{
						CMD_Offset.X = ParseNextFloat(&Cmd);
						CMD_Offset.Y = ParseNextFloat(&Cmd);
						CMD_Offset.Z = ParseNextFloat(&Cmd);
					}
					else if (ParseCommand(&Cmd, TEXT("SetScale")))
					{
						CMD_Scale.X = ParseNextFloat(&Cmd);
						CMD_Scale.Y = ParseNextFloat(&Cmd);
						CMD_Scale.Z = ParseNextFloat(&Cmd);
					}
					else if (ParseCommand(&Cmd, TEXT("SetRotation")))
					{
						CMD_RotOffset.Roll = appRound(ParseNextFloat(&Cmd) * (65536.0 / 360.0));
						CMD_RotOffset.Pitch = appRound(ParseNextFloat(&Cmd) * (65536.0 / 360.0));
						CMD_RotOffset.Yaw = appRound(ParseNextFloat(&Cmd) * (65536.0 / 360.0));
					}
					else if (ParseCommand(&Cmd, TEXT("SetBoundsMin")))
					{
						CMD_Bounds.Min.X = ParseNextFloat(&Cmd);
						CMD_Bounds.Min.Y = ParseNextFloat(&Cmd);
						CMD_Bounds.Min.Z = ParseNextFloat(&Cmd);
					}
					else if (ParseCommand(&Cmd, TEXT("SetBoundsMax")))
					{
						CMD_Bounds.Max.X = ParseNextFloat(&Cmd);
						CMD_Bounds.Max.Y = ParseNextFloat(&Cmd);
						CMD_Bounds.Max.Z = ParseNextFloat(&Cmd);
					}
					//else debugf(NAME_ExecWarning, TEXT("Unknown command: %ls"), Cmd);
				}
			}
		}
		if (IsVertexMesh())
		{
			for (i = 0; i < m_Frames.Num(); ++i)
			{
				CCpjFrmFrame& F = m_Frames(i);
				for (j = 0; j < F.m_PurePos.Num(); ++j)
					F.m_PurePos(j) = ToUECoords(F.m_PurePos(j));
			}
		}
		else
		{
			const INT numSkel = skm_Bones.Num();
			INT z;

			// Sort bones (root and parent bones must come first)!
			for (i = 0; i < (numSkel - 1); ++i)
			{
				z = i;
				for (j = (i + 1); j < numSkel; ++j)
				{
					if (skm_Bones(j).ParentIndex < skm_Bones(z).ParentIndex)
						z = j;
				}
				if (z != i)
					SwapBones(&skm_Bones(0), skm_Bones.Num(), i, z, skm_Verts);
			}

			for (i = 0; i < Animations.Num(); ++i)
			{
				FAnimSequence& A = Animations(i);
				/*if (A.AnimName != TEXT("IdleA"))
				{
					Animations.Remove(i--);
					continue;
				}*/

				for (j = 0; j < A.m_Frames.Num(); ++j)
				{
					CCpjSeqFrame& F = A.m_Frames(j);

					// Sort bones and transform them from DNF local pose -> world pose.
					{
						TArray<CCpjSeqScale> Tmp;
						Tmp.Empty(numSkel);
						Tmp.Add(numSkel);
						for (z = 0; z < numSkel; ++z)
						{
							Tmp(z).scale = FVector(1, 1, 1);
							Tmp(z).boneIndex = INDEX_NONE;
						}
						for (z = 0; z < F.scales.Num(); ++z)
						{
							INT iRef = FindBone(*A.m_BoneInfo(F.scales(z).boneIndex).name);
							if (iRef >= 0)
							{
								Tmp(iRef).scale = F.scales(z).scale;
								Tmp(iRef).boneIndex = z;
							}
						}
						ExchangeArray(Tmp, F.scales);
					}
					{
						TArray<CCpjSeqRotate> Tmp(numSkel);
						for (z = 0; z < numSkel; ++z)
						{
							Tmp(z).quat = FQuat::Identity;
							Tmp(z).axes = VAxes3();
							Tmp(z).boneIndex = INDEX_NONE;
						}
						for (z = 0; z < F.rotates.Num(); ++z)
						{
							INT iRef = FindBone(*A.m_BoneInfo(F.rotates(z).boneIndex).name);
							if (iRef >= 0)
							{
								Tmp(iRef).quat = F.rotates(z).quat;
								Tmp(iRef).axes = F.rotates(z).axes;
								Tmp(iRef).boneIndex = z;
							}
						}
						ExchangeArray(Tmp, F.rotates);

						for (z = 0; z < numSkel; ++z)
						{
							if (skm_Bones(z).parentBone)
							{
								F.rotates(z).quat = skm_Bones(z).baseCoords.r * F.rotates(z).quat;
								F.rotates(z).InvQuat = skm_Bones(z).baseCoords.r / F.rotates(z).quat;
							}
							else
							{
								F.rotates(z).quat = skm_Bones(z).baseCoords.r / F.rotates(z).quat;
								F.rotates(z).InvQuat = F.rotates(z).quat;
							}
						}
					}
					{
						TArray<CCpjSeqTranslate> Tmp;
						Tmp.Empty(numSkel);
						Tmp.Add(numSkel);
						FVector Delta;
						for (z = 0; z < numSkel; ++z)
						{
							Tmp(z).translate = FVector(0, 0, 0);
							Tmp(z).boneIndex = INDEX_NONE;
						}
						for (z = 0; z < F.translates.Num(); ++z)
						{
							INT iRef = FindBone(*A.m_BoneInfo(F.translates(z).boneIndex).name);
							if (iRef >= 0)
							{
								Delta = (F.translates(z).translate / F.scales(iRef).scale);
								Tmp(iRef).translate = Delta << skm_Bones(iRef).baseCoords.mr;
								Tmp(iRef).boneIndex = z;
							}
						}
						ExchangeArray(Tmp, F.translates);
					}

					// world pose -> UE pose
					for (z = 0; z < numSkel; ++z)
					{
						F.rotates(z).quat = ToUEQuat(F.rotates(z).quat);
						F.translates(z).translate = ToUEVector(F.translates(z).translate);
					}
				}
			}

			// Dnf verts -> UE verts
			for (i = 0; i < m_Verts.Num(); ++i)
			{
				CCpjGeoVert& V = m_Verts(i);
				V.refPosition = ToUEVector(V.refPosition);
			}

			// DNF bone coords -> world bone coords
			for (i = 0; i < numSkel; ++i)
			{
				CCpjSklBone& B = skm_Bones(i);
				if (B.parentBone)
					B.baseCoords.t = B.baseCoords.t << B.parentBone->GetParentAxes(); // DNF bone coords -> world coords
			}

			// DNF quat -> UE quat
			for (i = 0; i < numSkel; ++i)
			{
				CCpjSklBone& B = skm_Bones(i);
				B.baseCoords.r = ToUEQuat(B.baseCoords.r);
				B.baseCoords.t = ToUEVector(B.baseCoords.t);
			}

			// world bone coords -> UE bone coords
			for (i = 0; i < numSkel; ++i)
			{
				CCpjSklBone& B = skm_Bones(i);
				if (B.parentBone)
				{
					FCoords C = B.parentBone->GetParentFCoords();
					B.FinalPose = B.baseCoords.t.TransformVectorBy(C.Transpose());
					B.FinalQuat = B.baseCoords.r;
				}
				else
				{
					B.FinalPose = B.baseCoords.t;
					B.FinalQuat = B.baseCoords.r;
				}
			}
			for (i = 0; i < Animations.Num(); ++i)
			{
				FAnimSequence& A = Animations(i);
				for (j = 0; j < A.m_Frames.Num(); ++j)
				{
					CCpjSeqFrame& F = A.m_Frames(j);

					for (z = 0; z < numSkel; ++z)
					{
						CCpjSklBone& B = skm_Bones(z);
						if (B.parentBone)
						{
							FCoords C = F.GetParentFCoords(*B.parentBone);
							F.translates(z).FinalPose = B.FinalPose + F.translates(z).translate.TransformVectorBy(C.Transpose());
						}
						else F.translates(z).FinalPose = F.translates(z).translate;
					}
				}
			}
#if DEBUG_SINGLE_MESH && FALSE
			if (!appStricmp(*CurFile, DEBUG_MESH_NAME))
			{
				FStringOutputDevice StrT;
				StrT.Log(TEXT("Class MeshDebug extends Object;\r\n\r\nvar array<vector> V;\r\nvar array<string> B;\r\nvar Mesh Mesh;\r\n\r\ndefaultproperties\r\n{\r\n"));
				StrT.Logf(TEXT("\tMesh=%ls\r\n"), DEBUG_MESH_NAME);
				FVector Va, Vb;

				TArray<VCoords3> OutCoords(numSkel), DeltaCoords(numSkel);
				TArray<BYTE> InUse(numSkel);
				for (i = 0; i < numSkel; ++i)
				{
					OutCoords(i) = skm_Bones(i).baseCoords;
					DeltaCoords(i) = VCoords3();
					InUse(i) = FALSE;
				}
				for (i = 0; i < Animations.Num(); ++i)
				{
					FAnimSequence& A = Animations(i);
					if (A.AnimName == TEXT("Roar"))
					{
						CCpjSeqFrame& F = A.m_Frames(4);

						for (j = 0; j < F.translates.Num(); ++j)
						{
							if (F.translates(z).boneIndex >= 0)
							{
								InUse(j) = TRUE;
								DeltaCoords(j).t = F.translates(j).translate;
							}
						}
						for (j = 0; j < F.scales.Num(); ++j)
						{
							if (F.scales(z).boneIndex >= 0)
							{
								InUse(j) = TRUE;
								DeltaCoords(j).s = F.scales(j).scale;
							}
						}
						for (j = 0; j < F.rotates.Num(); ++j)
						{
							if (F.rotates(z).boneIndex >= 0)
							{
								InUse(j) = TRUE;
								DeltaCoords(j).mr >>= ToUEQuat(F.rotates(j).axes.GetQuat());
							}
						}
					}
				}
				for (i = 0; i < numSkel; ++i)
				{
					if (InUse(i))
						OutCoords(i) = DeltaCoords(i) << OutCoords(i);
					if (skm_Bones(i).parentBone)
					{
						OutCoords(i) <<= OutCoords(skm_Bones(i).ParentIndex);
					}
				}
				for (i = 0; i < numSkel; ++i)
				{
					CCpjSklBone& B = skm_Bones(i);
					if (B.parentBone)
					{
						Va = OutCoords(i).t;
						Vb = OutCoords(B.ParentIndex).t;

						Va = (Va * CMD_Scale) - CMD_Offset;
						Vb = (Vb * CMD_Scale) - CMD_Offset;
						StrT.Logf(TEXT("\tV.Add((X=%f,Y=%f,Z=%f))\r\n\tV.Add((X=%f,Y=%f,Z=%f))\r\n"), Va.X, Va.Y, Va.Z, Vb.X, Vb.Y, Vb.Z);
						StrT.Logf(TEXT("\tB.Add(\"%ls\")\r\n\tB.Add(\"%ls\")\r\n"), *B.name, *B.parentBone->name);
					}
				}
				StrT.Log(TEXT("}\r\n"));
				appSaveStringToFile(StrT, TEXT("C:\\Src\\Unreal\\test\\Classes\\MeshDebug.uc"));
			}
#endif
		}
	}
	static inline const TCHAR* ReadString(FArchive& Ar)
	{
		static TCHAR v[1024];
		TCHAR* p = v;
		BYTE ch;
		while (1)
		{
			Ar << ch;
			*p++ = ch;
			if (!ch)
				break;
		}
		return v;
	}
	UBOOL LoadFile(FArchive& Ar, const TCHAR* GroupName)
	{
		SCpjFileHeader header;
		Ar << header;
		debugfSlow(TEXT("riffMagic %ls"), DecodeMagic(header.riffMagic));
		debugfSlow(TEXT("formMagic %ls"), DecodeMagic(header.formMagic));
		if ((header.riffMagic != KRN_FOURCC(CPJ_HDR_RIFF_MAGIC))
			|| (header.formMagic != KRN_FOURCC(CPJ_HDR_FORM_MAGIC)))
		{
			warnf(TEXT("Failed to load CPJ file - Invalid header!"));
			return FALSE;
		}

		Clear();
		NDword i, j;
		SCpjFileHeader SubHrd;
		NDword position = sizeof(SCpjFileHeader);
		UBOOL bIsLoop = FALSE;
		while (1)
		{
			if (bIsLoop)
			{
				// set position to start of next chunk
				position += (SubHrd.lenFile + 8);
				if (SubHrd.lenFile & 1)
					position++;
			}
			else bIsLoop = TRUE;

			if (position >= (header.lenFile + 8))
				break;

			// seek to chunk position
			Ar.Seek(position);

			// read RIFF magic and length fields
			Ar << SubHrd;
			NDword timeStamp, ofsName;
			Ar << timeStamp << ofsName;

			if (ofsName)
			{
				Ar.Seek(position + ofsName);
				FString SecName = ReadString(Ar);
				debugfSlow(TEXT("Section %ls (%ls) Len %i Offset %i -> %i"), DecodeMagic(SubHrd.riffMagic), *SecName, INT(SubHrd.lenFile), INT(position), INT(position + SubHrd.lenFile + 8));

				Ar.Seek(position);
				const INT proxyLen = SubHrd.lenFile + 8;
				TArray<BYTE> fullData(proxyLen);
				Ar.Serialize(fullData.GetData(), proxyLen);

				if (SubHrd.riffMagic == KRN_FOURCC("SEQB"))
				{
					// verify header
					SSeqFile* file = reinterpret_cast<SSeqFile*>(fullData.GetData());
					if (file->header.version != 1)
						warnf(TEXT("Invalid SEQB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SSeqFrame* fileFrames = reinterpret_cast<SSeqFrame*>(&file->dataBlock[file->ofsFrames]);
					SSeqEvent* fileEvents = reinterpret_cast<SSeqEvent*>(&file->dataBlock[file->ofsEvents]);
					SSeqBoneInfo* fileBoneInfo = reinterpret_cast<SSeqBoneInfo*>(&file->dataBlock[file->ofsBoneInfo]);
					SSeqBoneTranslate* fileBoneTranslate = reinterpret_cast<SSeqBoneTranslate*>(&file->dataBlock[file->ofsBoneTranslate]);
					SSeqBoneRotate* fileBoneRotate = reinterpret_cast<SSeqBoneRotate*>(&file->dataBlock[file->ofsBoneRotate]);
					SSeqBoneScale* fileBoneScale = reinterpret_cast<SSeqBoneScale*>(&file->dataBlock[file->ofsBoneScale]);

					FAnimSequence* as = new (Animations) FAnimSequence();
					as->AnimName = SecName;

					// remove old array data
					as->m_Frames.Empty(file->numFrames); as->m_Frames.AddZeroed(file->numFrames);
					as->m_Events.Empty(file->numEvents); as->m_Events.AddZeroed(file->numEvents);
					as->m_BoneInfo.Empty(file->numBoneInfo); as->m_BoneInfo.AddZeroed(file->numBoneInfo);
					as->m_Rate = file->playRate;

					debugf(TEXT("Animation[%ls] Frames %i Events %i Bones %i"), *SecName, INT(file->numFrames), INT(file->numEvents), INT(file->numBoneInfo));

					// bone info
					for (i = 0; i < file->numBoneInfo; i++)
					{
						SSeqBoneInfo* iI = &fileBoneInfo[i];
						CCpjSeqBoneInfo* oI = &as->m_BoneInfo(i);
						oI->name = ReadStringByte(&file->dataBlock[iI->ofsName]);
						oI->srcLength = iI->srcLength;
					}

					// frames
					for (i = 0; i < file->numFrames; i++)
					{
						SSeqFrame* iF = &fileFrames[i];
						CCpjSeqFrame* oF = &as->m_Frames(i);
						if (iF->ofsVertFrameName != 0xFFFFFFFF)
							oF->vertFrameName = ReadStringByte(&file->dataBlock[iF->ofsVertFrameName]);
						oF->translates.Add(iF->numBoneTranslate);
						for (j = 0; j < iF->numBoneTranslate; j++)
						{
							SSeqBoneTranslate* iT = &fileBoneTranslate[iF->firstBoneTranslate + j];
							CCpjSeqTranslate* oT = &oF->translates(j);

#if EDIT_FILE_TEXT
							iT->translate = FVector(0, 0, 0);
							OutFile->Seek(position + (reinterpret_cast<BYTE*>(&iT->translate) - reinterpret_cast<BYTE*>(fullData.GetData())));
							FLOAT zf = 0.f;
							*OutFile << zf << zf << zf;
#endif

							oT->boneIndex = iT->boneIndex;
							oT->translate = iT->translate;
						}
						oF->rotates.Add(iF->numBoneRotate);
						for (j = 0; j < iF->numBoneRotate; j++)
						{
							SSeqBoneRotate* iR = &fileBoneRotate[iF->firstBoneRotate + j];
							CCpjSeqRotate* oR = &oF->rotates(j);
							oR->boneIndex = iR->boneIndex;
							
#if EDIT_FILE_TEXT
							iR->roll = 0;
							iR->pitch = 0;
							iR->yaw = 0;
							if (as->m_BoneInfo(iR->boneIndex).name == TEXT("ABDOMEN"))
								iR->yaw = 400 * i;
							//if (as->m_BoneInfo(iR->boneIndex).name == TEXT("CHEST"))
							//	iR->yaw = -400 * i;

							OutFile->Seek(position + (reinterpret_cast<BYTE*>(&iR->roll) - reinterpret_cast<BYTE*>(fullData.GetData())));
							*OutFile << iR->roll << iR->pitch << iR->yaw;
#endif

							oR->axes = ~VAxes3(FRotator(iR->pitch, iR->yaw, iR->roll));
							oR->quat = oR->axes.GetQuat();
						}
						oF->scales.Add(iF->numBoneScale);
						for (j = 0; j < iF->numBoneScale; j++)
						{
							SSeqBoneScale* iS = &fileBoneScale[iF->firstBoneScale + j];
							CCpjSeqScale* oS = &oF->scales(j);
							oS->boneIndex = iS->boneIndex;
							oS->scale = iS->scale;
						}
					}

					// events
					for (i = 0; i < file->numEvents; i++)
					{
						SSeqEvent* iE = &fileEvents[i];
						CCpjSeqEvent* oE = &as->m_Events(i);
						oE->eventType = iE->eventType;
						oE->time = iE->time;
						if (iE->ofsParam != 0xFFFFFFFF)
							oE->paramString = ReadStringByte(&file->dataBlock[iE->ofsParam]);
					}
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("MACB"))
				{
					SMacFile* file = reinterpret_cast<SMacFile*>(fullData.GetData());

					if (file->header.version != 1)
						warnf(TEXT("Invalid MACB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SMacSection* fileSections = reinterpret_cast<SMacSection*>(&file->dataBlock[file->ofsSections]);
					NDword* fileCommands = reinterpret_cast<NDword*>(&file->dataBlock[file->ofsCommands]);

					// remove old array data
					m_Sections.Empty(file->numSections);

					// sections
					for (i = 0; i < file->numSections; i++)
					{
						SMacSection* iS = &fileSections[i];
						CCpjMacSection* oS = new (m_Sections) CCpjMacSection;
						oS->name = ReadStringByte(&file->dataBlock[iS->ofsName]);
						oS->commands.Empty(iS->numCommands);
						for (j = 0; j < iS->numCommands; j++)
						{
							new (oS->commands) FString(ReadStringByte(&file->dataBlock[fileCommands[iS->firstCommand + j]]));
						}
					}
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("GEOB"))
				{
					if (m_Verts.Num() && SecName != GroupName)
						continue;

					// verify header
					SGeoFile* file = reinterpret_cast<SGeoFile*>(fullData.GetData());
					if (file->header.version != 1)
						warnf(TEXT("Invalid GEOB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SGeoVert* fileVerts = (SGeoVert*)(&file->dataBlock[file->ofsVertices]);
					SGeoEdge* fileEdges = (SGeoEdge*)(&file->dataBlock[file->ofsEdges]);
					SGeoTri* fileTris = (SGeoTri*)(&file->dataBlock[file->ofsTris]);
					SGeoMount* fileMounts = (SGeoMount*)(&file->dataBlock[file->ofsMounts]);
					NWord* fileObjLinks = (NWord*)(&file->dataBlock[file->ofsObjLinks]);

					// remove old array data
					m_Verts.Empty(file->numVertices); m_Verts.AddZeroed(file->numVertices);
					m_Edges.Empty(file->numEdges); m_Edges.AddZeroed(file->numEdges);
					m_Tris.Empty(file->numTris); m_Tris.AddZeroed(file->numTris);
					m_Mounts.Empty(file->numMounts); m_Mounts.AddZeroed(file->numMounts);

					debugf(TEXT("Geometry[%ls]: Verts %i Edges %i Tris %i Mounts %i"), *SecName, INT(file->numEdges), INT(file->numEdges), INT(file->numTris), INT(file->numMounts));

					// vertices
					for (i = 0; i < file->numVertices; i++)
					{
						SGeoVert* iV = &fileVerts[i];
						CCpjGeoVert* oV = &m_Verts(i);
						oV->iVertex = i;
						oV->flags = iV->flags;
						oV->groupIndex = iV->groupIndex;
						oV->edgeLinks.Add(iV->numEdgeLinks);
						for (j = 0; j < iV->numEdgeLinks; j++)
							oV->edgeLinks(j) = &m_Edges(fileObjLinks[iV->firstEdgeLink + j]);
						oV->triLinks.Add(iV->numTriLinks);
						for (j = 0; j < iV->numTriLinks; j++)
							oV->triLinks(j) = &m_Tris(fileObjLinks[iV->firstTriLink + j]);
						oV->refPosition = iV->refPosition;
					}

					// edges
					for (i = 0; i < file->numEdges; i++)
					{
						SGeoEdge* iE = &fileEdges[i];
						CCpjGeoEdge* oE = &m_Edges(i);
						oE->headVertex = &m_Verts(iE->headVertex);
						oE->tailVertex = &m_Verts(iE->tailVertex);
						oE->invertedEdge = &m_Edges(iE->invertedEdge);
						oE->triLinks.Add(iE->numTriLinks);
						for (j = 0; j < iE->numTriLinks; j++)
							oE->triLinks(j) = &m_Tris(fileObjLinks[iE->firstTriLink + j]);
					}

					// triangles
					for (i = 0; i < file->numTris; i++)
					{
						SGeoTri* iT = &fileTris[i];
						CCpjGeoTri* oT = &m_Tris(i);
						for (j = 0; j < 3; j++)
						{
							oT->edgeRing[j] = &m_Edges(iT->edgeRing[j]);
							oT->edgeRing[j]->tailVertex->RefCount = TRUE;
						}
					}

					// Check for unused vertices.
					for (i = 0; i < file->numVertices; i++)
					{
						CCpjGeoVert* v = &m_Verts(i);
						if (!v->RefCount)
							debugf(TEXT("Vertex %i/%i is UNUSED"), i, file->numVertices);
					}

					// mount points
					for (i = 0; i < file->numMounts; i++)
					{
						SGeoMount* iM = &fileMounts[i];
						CCpjGeoMount* oM = &m_Mounts(i);
						StrByteCopy(oM->name, &file->dataBlock[iM->ofsName], 64);
						oM->triIndex = iM->triIndex;
						oM->triBarys = iM->triBarys;
						oM->baseCoords = VCoords3(iM->baseRotate, iM->baseTranslate, iM->baseScale);
					}
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("SRFB"))
				{
					if (sm_Tris.Num() && SecName != GroupName)
						continue;

					// verify header
					SSrfFile* file = reinterpret_cast<SSrfFile*>(fullData.GetData());
					if (file->header.version != 1)
						warnf(TEXT("Invalid SRFB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SSrfTex* fileTextures = reinterpret_cast<SSrfTex*>(&file->dataBlock[file->ofsTextures]);
					SSrfTri* fileTris = reinterpret_cast<SSrfTri*>(&file->dataBlock[file->ofsTris]);
					SSrfUV* fileUV = reinterpret_cast<SSrfUV*>(&file->dataBlock[file->ofsUV]);

					debugf(TEXT("Surface[%ls]: Textures %i Tris %i UV-Maps %i"), *SecName, INT(file->numTextures), INT(file->numTris), INT(file->numUV));

					// remove old array data
					m_Textures.Empty(file->numTextures); m_Textures.Add(file->numTextures);
					sm_Tris.Empty(file->numTris); sm_Tris.Add(file->numTris);
					m_UV.Empty(file->numUV); m_UV.Add(file->numUV);

					// textures
					for (i = 0; i < file->numTextures; i++)
					{
						StrByteCopy(m_Textures(i).name, &file->dataBlock[fileTextures[i].ofsName], 128);
						StrByteCopy(m_Textures(i).refName, &file->dataBlock[fileTextures[i].ofsRefName], 128);

						TCHAR* s = appStrstr(m_Textures(i).name, TEXT(".")); // Strip file extension!
						if (s)
							*s = 0;
					}

					// triangles
					for (i = 0; i < file->numTris; i++)
					{
						SSrfTri* iT = &fileTris[i];
						CCpjSrfTri* oT = &sm_Tris(i);
						for (NDword j = 0; j < 3; j++)
							oT->uvIndex[j] = iT->uvIndex[j];
						oT->texIndex = iT->texIndex;
						oT->flags = iT->flags;
						oT->smoothGroup = iT->smoothGroup;
						oT->alphaLevel = iT->alphaLevel;
						oT->glazeTexIndex = iT->glazeTexIndex;
						oT->glazeFunc = iT->glazeFunc;
					}

					// texture vertex UVs
					for (i = 0; i < file->numUV; i++)
						m_UV(i).Set(fileUV[i].u, fileUV[i].v);
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("LODB"))
				{
					// verify header
					SLodFile* file = reinterpret_cast<SLodFile*>(fullData.GetData());
					if (file->header.version < 3)
					{
						// old LOD format, ignore contents
						debugf(TEXT("LOD[%ls]: Disabled!"), *SecName);
					}
					else
					{
						// set up image data pointers
						SLodLevel* fileLevels = reinterpret_cast<SLodLevel*>(&file->dataBlock[file->ofsLevels]);
						NWord* fileVertRelay = reinterpret_cast<NWord*>(&file->dataBlock[file->ofsVertRelay]);
						SLodTri* fileTriangles = reinterpret_cast<SLodTri*>(&file->dataBlock[file->ofsTriangles]);

						debugf(TEXT("LOD[%ls]: NumLodLevels %i"), *SecName, INT(file->numLevels));

						// remove old array data
						m_Levels.Empty(file->numLevels); m_Levels.AddZeroed(file->numLevels);

						// levels
						for (i = 0; i < file->numLevels; i++)
						{
							SLodLevel* iL = &fileLevels[i];
							CCpjLodLevel* oL = &m_Levels(i);

							oL->detail = iL->detail;
							oL->vertRelay.Add(iL->numVertRelay);
							for (j = 0; j < iL->numVertRelay; j++)
								oL->vertRelay(j) = fileVertRelay[iL->firstVertRelay + j];
							oL->triangles.Add(iL->numTriangles);
							for (j = 0; j < iL->numTriangles; j++)
							{
								SLodTri* iT = &fileTriangles[iL->firstTriangle + j];
								CCpjLodTri* oT = &oL->triangles(j);
								oT->srfTriIndex = iT->srfTriIndex;
								oT->vertIndex[0] = iT->vertIndex[0];
								oT->vertIndex[1] = iT->vertIndex[1];
								oT->vertIndex[2] = iT->vertIndex[2];
								oT->uvIndex[0] = iT->uvIndex[0];
								oT->uvIndex[1] = iT->uvIndex[1];
								oT->uvIndex[2] = iT->uvIndex[2];
							}
						}
					}
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("SKLB"))
				{
					if (skm_Bones.Num() && SecName != GroupName)
						continue;

					// verify header
					SSklFile* file = reinterpret_cast<SSklFile*>(fullData.GetData());
					if (file->header.version != 1)
						warnf(TEXT("Invalid SKLB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SSklBone* fileBones = (SSklBone*)(&file->dataBlock[file->ofsBones]);
					SSklVert* fileVerts = (SSklVert*)(&file->dataBlock[file->ofsVerts]);
					SSklWeight* fileWeights = (SSklWeight*)(&file->dataBlock[file->ofsWeights]);
					SSklMount* fileMounts = (SSklMount*)(&file->dataBlock[file->ofsMounts]);

					debugf(TEXT("SkeletalData[%ls]: Bones %i WeightedVerts %i SkeletalMounts %i"), *SecName, INT(file->numBones), INT(file->numVerts), INT(file->numMounts));

					// remove old array data
					skm_Bones.Empty(file->numBones); skm_Bones.AddZeroed(file->numBones);
					skm_Verts.Empty(file->numVerts); skm_Verts.AddZeroed(file->numVerts);
					skm_Mounts.Empty(file->numMounts); skm_Mounts.AddZeroed(file->numMounts);

					// bones
					for (i = 0; i < file->numBones; i++)
					{
						SSklBone* iB = &fileBones[i];
						CCpjSklBone* oB = &skm_Bones(i);
						oB->Index = i;
						oB->name = ReadStringByte(&file->dataBlock[iB->ofsName]);
						if (!oB->name.Len())
							oB->name = FString::Printf(TEXT("Bone_%i"), INT(i));
						oB->parentBone = NULL;
						if (iB->parentIndex != 0xFFFFFFFF)
							oB->parentBone = &skm_Bones(iB->parentIndex);
						oB->baseCoords = VCoords3(iB->baseRotate, iB->baseTranslate, iB->baseScale);
						oB->length = iB->length;

						/*FRotator R = oB->baseCoords.r;
						FQuat Q = R.Quaternion();
						debugf(TEXT("Bone %ls %i,%i,%i (%f,%f,%f,%f)(%f,%f,%f,%f)"), *oB->name, R.Yaw, R.Pitch, R.Roll, oB->baseCoords.r.X, oB->baseCoords.r.Y, oB->baseCoords.r.Z, oB->baseCoords.r.W, Q.X, Q.Y, Q.Z, Q.W);*/
					}

					// Assign parent index!
					for (i = 0; i < file->numBones; i++)
					{
						CCpjSklBone& oB = skm_Bones(i);
						if (oB.parentBone)
							oB.ParentIndex = oB.parentBone->Index;
						else oB.ParentIndex = INDEX_NONE;
					}

					// vertices
					for (i = 0; i < file->numVerts; i++)
					{
						SSklVert* iV = &fileVerts[i];
						CCpjSklVert* oV = &skm_Verts(i);
						oV->weights.Add(iV->numWeights);
						for (j = 0; j < iV->numWeights; j++)
						{
							SSklWeight* iW = &fileWeights[iV->firstWeight + j];
							CCpjSklWeight* oW = &oV->weights(j);
							oW->BoneIndex = iW->boneIndex;
							oW->factor = iW->weightFactor;
							oW->offsetPos = iW->offsetPos;
						}
					}

					// mounts
					for (i = 0; i < file->numMounts; i++)
					{
						SSklMount* iM = &fileMounts[i];
						CCpjSklMount* oM = &skm_Mounts(i);
						oM->name = ReadStringByte(&file->dataBlock[iM->ofsName]);
						oM->bone = NULL;
						if (iM->boneIndex != 0xFFFFFFFF)
							oM->bone = &skm_Bones(iM->boneIndex);
						oM->baseCoords = VCoords3(iM->baseRotate, iM->baseTranslate, iM->baseScale);
					}
				}
				else if (SubHrd.riffMagic == KRN_FOURCC("FRMB"))
				{
					if (m_Frames.Num() && SecName != GroupName)
						continue;

					// verify header
					SFrmFile* file = reinterpret_cast<SFrmFile*>(fullData.GetData());
					if (file->header.version != 1)
						warnf(TEXT("Invalid FRMB ver (%i)"), INT(file->header.version));

					// set up image data pointers
					SFrmFrame* fileFrames = reinterpret_cast<SFrmFrame*>(&file->dataBlock[file->ofsFrames]);

					// remove old array data
					m_Frames.Empty(file->numFrames); m_Frames.AddZeroed(file->numFrames);

					// bounding box
					m_Bounds = FBox(file->bbMin, file->bbMax);

					// frames
					for (i = 0; i < file->numFrames; i++)
					{
						SFrmFrame* iF = &fileFrames[i];
						CCpjFrmFrame* oF = &m_Frames(i);

						oF->m_Name = ReadStringByte(&file->dataBlock[iF->ofsFrameName]);

						// bounding box
						oF->m_Bounds = FBox(iF->bbMin, iF->bbMax);

						// set up image data pointers
						SFrmGroup* fileGroups = reinterpret_cast<SFrmGroup*>(&file->dataBlock[iF->ofsGroups]);
						FVector* fileVertsPure = reinterpret_cast<FVector*>(&file->dataBlock[iF->ofsVerts]);
						SFrmBytePos* fileVertsByte = reinterpret_cast<SFrmBytePos*>(&file->dataBlock[iF->ofsVerts]);

						oF->m_isCompressed = (iF->numGroups != 0);

						if (oF->m_isCompressed)
						{
							// groups
							oF->m_Groups.Add(iF->numGroups);
							for (j = 0; j < iF->numGroups; j++)
							{
								oF->m_Groups(j).scale = fileGroups[j].byteScale;
								oF->m_Groups(j).translate = fileGroups[j].byteTranslate;
							}

							// vertices
							oF->m_BytePos.Add(iF->numVerts);

							memcpy(&oF->m_BytePos(0), &fileVertsByte[0], iF->numVerts * sizeof(SFrmBytePos));

							// build pure positions
							CCpjFrmBytePos* b;
							CCpjFrmGroup* g;
							FVector* v;
							oF->m_PurePos.Add(iF->numVerts);
							for (j = 0; j < iF->numVerts; j++)
							{
								v = &oF->m_PurePos(j);
								b = &oF->m_BytePos(j);
								g = &oF->m_Groups(b->group);
								v->X = (b->pos[0] * g->scale.X) + g->translate.X;
								v->Y = (b->pos[1] * g->scale.Y) + g->translate.Y;
								v->Z = (b->pos[2] * g->scale.Z) + g->translate.Z;
							}

							// finish up
							oF->m_BytePos.Empty();
							oF->m_Groups.Empty();
							oF->m_isCompressed = FALSE;
						}
						else
						{
							// vertices
							oF->m_PurePos.Add(iF->numVerts);

							memcpy(&oF->m_PurePos(0), &fileVertsPure[0], iF->numVerts * sizeof(FVector));
						}
					}
				}
				else debugf(TEXT("UNKNOWN SECTION: %ls Len %i"), DecodeMagic(SubHrd.riffMagic), INT(SubHrd.lenFile));
			}
		}
		return TRUE;
	}
	UBOOL LoadChunked(const TCHAR* File, const TCHAR* ChunkID)
	{
#if EDIT_FILE_TEXT
		FString OutputFile = FString(File) + TEXT(".txt");
		if (GFileManager->FileSize(*OutputFile) < 0)
		{
			GWarn->Logf(TEXT("OrginalFile %ls not found!"), *OutputFile);
			return FALSE;
		}
		OutFile = GFileManager->CreateFileWriter(File, FILEWRITE_Append, GWarn);
		if (!OutFile)
			return FALSE;
		FArchive* Ar = GFileManager->CreateFileReader(*OutputFile, 0, GWarn);
#else
		FArchive* Ar = GFileManager->CreateFileReader(File, 0, GWarn);
#endif
		if (!Ar)
			return FALSE;

		UBOOL bResult = LoadFile(*Ar, ChunkID);
		delete Ar;
#if EDIT_FILE_TEXT
		delete OutFile;
#endif

		if (bResult)
		{
			if (!m_Textures.Num())
			{
				CCpjSrfTex* defMat = new(m_Textures) CCpjSrfTex();
				appStrcpy(defMat->name, TEXT("DefaultTexture"));
				appStrcpy(defMat->refName, TEXT("DefaultTexture"));
			}
			TransformToUE();
			if (IsVertexMesh())
				CalcBestScaling();
		}
		return bResult;
	}
public:
	UBOOL LoadMesh(const TCHAR* File, const TCHAR* MeshName)
	{
		CurFile = MeshName;
		const TCHAR* Str = File;
		const TCHAR* fStr = NULL;
		while (*Str)
		{
			if (*Str == '\\')
				fStr = Str;
			++Str;
		}

		if (!fStr)
			return FALSE;

		FString FStr(File, fStr);
		return LoadChunked(*FStr, (fStr + 1));
	}

	inline UBOOL IsAnimatedMesh() const
	{
		return (Animations.Num() > 0);
	}
	inline UBOOL IsVertexMesh() const
	{
		return (skm_Bones.Num() == 0);
	}
	inline UBOOL HasExportedTex(const TCHAR* Tex)
	{
		return ExportedTex.FindItemIndex(Tex) != INDEX_NONE;
	}
	inline void ExportTexInfo(const TCHAR* MeshName, FOutputDevice& Ar)
	{
		Ar.Log(TEXT("\r\n"));
		INT i;
		if (IsVertexMesh())
		{
			for (i = 0; i < m_Textures.Num(); ++i)
			{
				if (!HasExportedTex(m_Textures(i).name))
				{
					new (ExportedTex) FString(m_Textures(i).name);
					Ar.Logf(TEXT("#exec TEXTURE IMPORT NAME=%ls FILE=\"Textures\\%ls.pcx\" GROUP=Skins\r\n"), m_Textures(i).name, m_Textures(i).name);
				}
			}
			for (i = 0; i < m_Textures.Num(); ++i)
				Ar.Logf(TEXT("#exec MESHMAP SETTEXTURE MESHMAP=%ls NUM=%i TEXTURE=%ls\r\n"), MeshName, i, m_Textures(i).name);
		}
		else
		{
			for (i = 0; i < PSK_Materials.Num(); ++i)
			{
				TCHAR* n = appFromAnsi(PSK_Materials(i).MaterialName);
				if (!HasExportedTex(n))
				{
					new (ExportedTex) FString(n);
					Ar.Logf(TEXT("#exec TEXTURE IMPORT NAME=%ls FILE=\"Textures\\%ls.pcx\" GROUP=Skins\r\n"), n, n);
				}
			}
			for (i = 0; i < PSK_Materials.Num(); ++i)
				Ar.Logf(TEXT("#exec MESHMAP SETTEXTURE MESHMAP=%ls NUM=%i TEXTURE=%ls\r\n"), MeshName, i, appFromAnsi(PSK_Materials(i).MaterialName));
		}
		Ar.Log(TEXT("\r\n"));
	}

	void ExportPSK(FArchive& Ar);
	void ExportPSA(FArchive& Ar);

	void Export3DD(FArchive& Ar);
	void Export3DA(FArchive& Ar);

	void ExportMeshInfo(const TCHAR* MeshName, FOutputDevice& Ar)
	{
		debugf(TEXT("Export UC info..."));
		Ar.Logf(TEXT("/* ============ %ls ============ */\r\n"), MeshName);
		if (CMD_Author.Len() && CMD_Author != TEXT("Unknown"))
			Ar.Logf(TEXT("/* %ls */\r\n"), *CMD_Author);
		INT i;

		if (IsVertexMesh())
		{
			Ar.Logf(TEXT("#exec MESH IMPORT MESH=%ls ANIVFILE=\"Models\\%ls_a.3d\" DATAFILE=\"Models\\%ls_d.3d\""), MeshName, MeshName, MeshName);
			if (IsAnimatedMesh())
			{
				Ar.Log(TEXT("\r\n"));
				Ar.Logf(TEXT("#exec MESH LODPARAMS MESH=%ls STRENGTH=0.1\r\n\r\n"), MeshName);
			}
			else Ar.Log(TEXT(" MLOD=0\r\n"));
			{
				FVector V = (CMD_Offset - VertOrigin) * VertScaling;
				Ar.Logf(TEXT("#exec MESH ORIGIN MESH=%ls X=%.3f Y=%.3f Z=%.3f YAW=0\r\n"), MeshName, V.X, V.Y, V.Z);
			}
			{
				INT numFrames = 0;
				for (i = 0; i < Animations.Num(); ++i)
					numFrames += Animations(i).m_Frames.Num();
				numFrames = Max(numFrames, 1);
				Ar.Logf(TEXT("#exec MESH SEQUENCE MESH=%ls SEQ=All\tSTARTFRAME=0 NUMFRAMES=%i\r\n"), MeshName, numFrames);
				numFrames = 0;
				for (i = 0; i < Animations.Num(); ++i)
				{
					Ar.Logf(TEXT("#exec MESH SEQUENCE MESH=%ls SEQ=%ls\tSTARTFRAME=%i NUMFRAMES=%i RATE=%g\r\n"), MeshName, *Animations(i).AnimName, numFrames, Animations(i).m_Frames.Num(), Animations(i).m_Rate);
					numFrames += Animations(i).m_Frames.Num();
				}
			}
			{
				FVector V(1.f / VertScaling.X, 1.f / VertScaling.Y, 1.f / VertScaling.Z);
				V *= CMD_Scale;
				Ar.Logf(TEXT("#exec MESHMAP SCALE MESHMAP=%ls X=%.3f Y=%.3f Z=%.3f\r\n"), MeshName, V.X, V.Y, V.Z);
			}
			ExportTexInfo(MeshName, Ar);
		}
		else
		{
			if (IsAnimatedMesh())
			{
				{
					INT numFrames = 0;
					for (i = 0; i < Animations.Num(); ++i)
					{
						numFrames += Animations(i).m_Frames.Num();
					}
					Ar.Logf(TEXT("#exec ANIM IMPORT ANIM=%lsAnim ANIMFILE=\"Models\\%ls.psa\" COMPRESS=1 MAXKEYS=%i\r\n\r\n"), MeshName, MeshName, numFrames);
				}
				{
					Ar.Logf(TEXT("#exec ANIM SEQUENCE ANIM=%lsAnim SEQ=All STARTFRAME=0 NUMFRAMES=1 RATE=30 COMPRESS=1.00\r\n"), MeshName);
					INT Offset = 0;
					for (INT i = 0; i < Animations.Num(); ++i)
					{
						const FAnimSequence& a = Animations(i);
						Ar.Logf(TEXT("#exec ANIM SEQUENCE ANIM=%lsAnim SEQ=%ls STARTFRAME=%i NUMFRAMES=%i RATE=%g COMPRESS=1.00\r\n"), MeshName, *a.AnimName, Offset, a.m_Frames.Num(), a.m_Rate);
						Offset += a.m_Frames.Num();
					}
					Ar.Logf(TEXT("#exec ANIM DIGEST ANIM=%lsAnim\r\n\r\n"), MeshName);
				}
			}
			Ar.Logf(TEXT("#exec MESH MODELIMPORT MESH=%ls MODELFILE=\"Models\\%ls.psk\"\r\n"), MeshName, MeshName);
			Ar.Logf(TEXT("#exec MESH LODPARAMS MESH=%ls STRENGTH=0.1\r\n"), MeshName);
			Ar.Logf(TEXT("#exec MESH ORIGIN MESH=%ls X=%.3f Y=%.3f Z=%.3f YAW=0 PITCH=0 ROLL=0\r\n"), MeshName, CMD_Offset.X, CMD_Offset.Y, CMD_Offset.Z);
			Ar.Logf(TEXT("#exec MESHMAP SCALE MESHMAP=%ls X=%.3f Y=%.3f Z=%.3f\r\n"), MeshName, CMD_Scale.X, CMD_Scale.Y, CMD_Scale.Z);
			Ar.Logf(TEXT("#exec MESH BOUNDINGBOX MESH=%ls XMIN=%g YMIN=%g ZMIN=%g XMAX=%g YMAX=%g ZMAX=%g\r\n"), MeshName, CMD_Bounds.Min.X, CMD_Bounds.Min.Y, CMD_Bounds.Min.Z, CMD_Bounds.Max.X, CMD_Bounds.Max.Y, CMD_Bounds.Max.Z);
			{
				FVector Center, Extent;
				m_Bounds.GetCenterAndExtents(Center, Extent);
				FLOAT Dist = Extent.Size();
				Ar.Logf(TEXT("#exec MESH BOUNDINGSPHERE MESH=%ls X=%g Y=%g Z=%g W=%g\r\n"), MeshName, Center.X, Center.Y, Center.Z, Dist);
			}
			ExportTexInfo(MeshName, Ar);
			if (IsAnimatedMesh())
				Ar.Logf(TEXT("#exec MESH DEFAULTANIM MESH=%ls ANIM=%lsAnim\r\n\r\n"), MeshName, MeshName);
		}
	}
};

struct VVertex
{
	_WORD	PointIndex;	 // Index to a point.
	FLOAT   U, V;         // Scaled to BYTES, rather...-> Done in digestion phase, on-disk size doesn't matter here.
	BYTE    MatIndex;    // At runtime, this one will be implied by the face that's pointing to us.
	BYTE    Reserved;    // Top secret.
};
struct VTriangle
{
	_WORD   WedgeIndex[3];	 // point to three vertices in the vertex list.
	BYTE    MatIndex;	     // Materials can be anything.
	BYTE    AuxMatIndex;     // Second material (eg. damage skin, shininess, detail texture / detail mesh...
	DWORD   SmoothingGroups; // 32-bit flag for smoothing groups AND Lod-bias calculation.
};
struct VJointPos
{
	FQuat   	Orientation;  //
	FVector		Position;     //  needed or not ?

	FLOAT       Length;       //  For collision testing / debugging drawing...
	FLOAT       XSize;
	FLOAT       YSize;
	FLOAT       ZSize;
};
struct VBone
{
	ANSICHAR    Name[64];     //
	DWORD		Flags;        // reserved / 0x02 = bone where skin is to be attached...	
	INT 		NumChildren;  // children  // only needed in animation ?
	INT         ParentIndex;  // 0/NULL if this is the root bone.  
	VJointPos	BonePos;      // reference position
};
struct VRawBoneInfluence // just weight, vertex, and Bone, sorted later....
{
	FLOAT Weight;
	INT   PointIndex;
	INT   BoneIndex;
};

inline void CopyStringToAnsi(ANSICHAR* Dest, const TCHAR* Src, INT MaxLen)
{
	INT i = 0;
	for (; i < (MaxLen - 1); ++i)
	{
		if (!Src[i])
			break;
		Dest[i] = ToAnsi(Src[i]);
	}
	for (; i < MaxLen; ++i)
		Dest[i] = 0;
}

struct VChunkHeader
{
	ANSICHAR	ChunkID[20];  // string ID of up to 19 chars (usually zero-terminated)
	INT			TypeFlag;     // Flags/reserved
	INT         DataSize;     // size per struct following;
	INT         DataCount;    // number of structs/
};

struct FMaterialCache
{
private:
	struct FMatPair
	{
		BYTE Flags;
		FString MatName;

		FMatPair(BYTE f, const TCHAR* mn)
			: Flags(f), MatName(mn)
		{}
	};
	TArray<FMatPair> Materials;
	TArray<VMaterial>& OutMat;

public:
	FMaterialCache(TArray<VMaterial>& M)
		: OutMat(M)
	{}
	inline BYTE GetMaterial(BYTE Flags, const TCHAR* MatName)
	{
		for (INT i = 0; i < Materials.Num(); ++i)
			if (Materials(i).Flags == Flags && Materials(i).MatName == MatName)
				return i;

		BYTE Ix = OutMat.Num();
		VMaterial* M = new (OutMat) VMaterial();
		M->AuxFlags = 0;
		M->AuxMaterial = 0;
		M->LodBias = 1;
		M->LodStyle = 0;
		CopyStringToAnsi(M->MaterialName, MatName, ARRAY_COUNT(VMaterial::MaterialName));
		M->TextureIndex = Ix;
		M->PolyFlags = Flags;
		new (Materials) FMatPair(Flags, MatName);
		return Ix;
	}
};

#define WriteChunk(Serializer,Struct,VarArray,ChunkName) \
	Chunk.DataSize = sizeof(Struct); \
	Chunk.DataCount = VarArray.Num(); \
	CopyStringToAnsi(Chunk.ChunkID,ChunkName,ARRAY_COUNT(Chunk.ChunkID)); \
	Serializer.Serialize(&Chunk,sizeof(VChunkHeader)); \
	if( VarArray.Num() ) \
		Serializer.Serialize(VarArray.GetData(),sizeof(Struct)*VarArray.Num());

void DnfMesh::ExportPSK(FArchive& Ar)
{
	debugf(TEXT("Export PSK..."));
	CCpjLodLevel* mLod = NULL;
	if (m_Levels.Num())
		mLod = &m_Levels(m_Levels.Num() - 1);
	const INT NumVerts = mLod ? mLod->vertRelay.Num() : m_Verts.Num();

	// Init arrays
	TArray<FVector> Points(NumVerts);
	TArray<VVertex> Wedges;
	TArray<VTriangle> Tris(m_Tris.Num());
	check(m_Tris.Num() == sm_Tris.Num());
	TArray<VBone> Bones(skm_Bones.Num());
	PSK_Materials.Empty();
	TArray<VRawBoneInfluence> Influences;

	// Collect data into arrays.
	INT i, j;
	if (NumVerts > m_Verts.Num())
		warnf(TEXT("Vertex count mismatch Relay %i Geometry %i"), NumVerts, m_Verts.Num());
	for (i = 0; i < NumVerts; ++i)
		Points(i) = m_Verts(i).refPosition;
	{
		FMaterialCache mCache(PSK_Materials);
		UBOOL bWarnedBounds = FALSE;
		for (i = 0; i < sm_Tris.Num(); ++i)
		{
			VTriangle& V = Tris(i);
			const CCpjGeoTri& GW = m_Tris(i);
			const CCpjSrfTri& RW = sm_Tris(i);

			V.AuxMatIndex = 0;
			V.MatIndex = mCache.GetMaterial(RW.GetMTFlags(), m_Textures.IsValidIndex(RW.texIndex) ? m_Textures(RW.texIndex).name : TEXT("DefaultTexture"));
			V.SmoothingGroups = RW.smoothGroup;
			V.WedgeIndex[0] = Wedges.Num();
			V.WedgeIndex[1] = Wedges.Num() + 1;
			V.WedgeIndex[2] = Wedges.Num() + 2;

			for (j = 0; j < 3; ++j)
			{
				VVertex* W = new (Wedges) VVertex;
				W->MatIndex = V.MatIndex;
				W->PointIndex = GW.edgeRing[j]->tailVertex->iVertex;
				check(Points.IsValidIndex(W->PointIndex));
				W->U = m_UV(RW.uvIndex[j]).U;
				W->V = m_UV(RW.uvIndex[j]).V;
			}
		}
		if (!PSK_Materials.Num())
		{
			VMaterial* M = new (PSK_Materials) VMaterial();
			M->AuxFlags = 0;
			M->AuxMaterial = 0;
			M->LodBias = 1;
			M->LodStyle = 0;
			CopyStringToAnsi(M->MaterialName, TEXT("DefaultTexture"), ARRAY_COUNT(VMaterial::MaterialName));
			M->TextureIndex = 0;
			M->PolyFlags = 0;
		}
	}
	{
		if (!Bones.Num())
		{
			VBone* B = new (Bones) VBone();
			B->BonePos.Orientation = FQuat::Identity;
			B->BonePos.Position = FVector(0, 0, 0);
			B->Flags = 0;
			CopyStringToAnsi(B->Name, TEXT("Root"), ARRAY_COUNT(VBone::Name));
			B->NumChildren = 0;
			B->ParentIndex = 0;
		}
		else
		{
			for (i = 0; i < skm_Bones.Num(); ++i)
			{
				VBone& B = Bones(i);
				CCpjSklBone& RB = skm_Bones(i);
				B.BonePos.Orientation = RB.FinalQuat;
				B.BonePos.Position = RB.FinalPose;
				B.Flags = 0;
				CopyStringToAnsi(B.Name, *RB.name, ARRAY_COUNT(B.Name));
				INT numCh = 0;
				for (j = 0; j < skm_Bones.Num(); ++j)
					if (i != j && skm_Bones(j).parentBone == &RB)
						++numCh;
				B.NumChildren = numCh;
				B.ParentIndex = RB.parentBone ? RB.parentBone->Index : 0;
			}
		}
	}
	{
		const CCpjSklVert* iv = &skm_Verts(0);
		const INT SkmCount = skm_Verts.Num();

		if (mLod)
		{
			NWord* lodRelay = &mLod->vertRelay(0);
			const INT MaxRelay = Min(mLod->vertRelay.Num(), NumVerts);
			for (i = 0; i < MaxRelay; i++)
			{
				if (lodRelay[i] >= SkmCount)
				{
					VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
					inf->BoneIndex = 0;
					inf->PointIndex = i;
					inf->Weight = 1.f;
					continue;
				}
				//const CCpjSklVert& xv = iv[lodRelay[i]];
				const CCpjSklVert& xv = iv[i];
				const INT wcount = xv.weights.Num();
				const CCpjSklWeight* w = &xv.weights(0);
				if (!wcount)
				{
					VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
					inf->BoneIndex = 0;
					inf->PointIndex = i;
					inf->Weight = 1.f;
				}
				else
				{
					for (j = 0; j < wcount; j++)
					{
						VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
						inf->BoneIndex = w[j].BoneIndex;
						inf->PointIndex = i;
						if (wcount == 1)
							inf->Weight = 1.f;
						else inf->Weight = w[j].factor;
					}
				}
			}
		}
		else
		{
			const INT MaxSk = Min(SkmCount, NumVerts);
			for (i = 0; i < MaxSk; i++)
			{
				const CCpjSklVert& xv = iv[i];
				const INT wcount = xv.weights.Num();
				const CCpjSklWeight* w = &xv.weights(0);
				if (!wcount)
				{
					VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
					inf->BoneIndex = 0;
					inf->PointIndex = i;
					inf->Weight = 1.f;
				}
				else
				{
					for (j = 0; j < wcount; j++)
					{
						VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
						inf->BoneIndex = w[j].BoneIndex;
						inf->PointIndex = i;
						if (wcount == 1)
							inf->Weight = 1.f;
						else inf->Weight = w[j].factor;
					}
				}
			}
		}
		for (; i < NumVerts; ++i)
		{
			VRawBoneInfluence* inf = new(Influences)VRawBoneInfluence();
			inf->BoneIndex = 0;
			inf->PointIndex = i;
			inf->Weight = 1.f;
		}
	}

	// Serialize everything.
	VChunkHeader Chunk; // Main header.
	appMemzero(&Chunk, sizeof(VChunkHeader));
	CopyStringToAnsi(Chunk.ChunkID, TEXT("ACTRHEAD"), ARRAY_COUNT(Chunk.ChunkID));
	Chunk.TypeFlag = 1999801;
	Ar.Serialize(&Chunk, sizeof(VChunkHeader));

	// Write PSK specific data.
	WriteChunk(Ar, FVector, Points, TEXT("PNTS0000")); // Points.
	WriteChunk(Ar, VVertex, Wedges, TEXT("VTXW0000")); // Wedges.
	WriteChunk(Ar, VTriangle, Tris, TEXT("FACE0000")); // Faces.
	WriteChunk(Ar, VMaterial, PSK_Materials, TEXT("MATT0000")); // Materials.
	WriteChunk(Ar, VBone, Bones, TEXT("REFSKELT")); // Bones data.
	WriteChunk(Ar, VRawBoneInfluence, Influences, TEXT("RAWWEIGHTS")); // Influences.
}

// Binary bone format to deal with raw animations as generated by various exporters.
struct FNamedBoneBinary
{
	ANSICHAR   Name[64];	// Bone's name
	DWORD      Flags;		// reserved
	INT        NumChildren; //
	INT		   ParentIndex;	// 0/NULL if this is the root bone.  
	VJointPos  BonePos;	    //
};
// Binary animation info format - used to organize raw animation keys into FAnimSeqs on rebuild
// Similar to MotionChunkDigestInfo..
struct AnimInfoBinary
{
	ANSICHAR Name[64];     // Animation's name
	ANSICHAR Group[64];    // Animation's group name	

	INT TotalBones;           // TotalBones * NumRawFrames is number of animation keys to digest.

	INT RootInclude;          // 0 none 1 included 		
	INT KeyCompressionStyle;  // Reserved: variants in tradeoffs for compression.
	INT KeyQuotum;            // Max key quotum for compression	
	FLOAT KeyReduction;       // desired 
	FLOAT TrackTime;            // explicit - can be overridden by the animation rate
	FLOAT AnimRate;           // frames per second.
	INT StartBone;            // - Reserved: for partial animations.
	INT FirstRawFrame;        //
	INT NumRawFrames;         // NumRawFrames and AnimRate dictate tracktime...
};
// An animation key.
struct VQuatAnimKey
{
	FVector		Position;           // relative to parent.
	FQuat       Orientation;        // relative to parent.
	FLOAT       Time;				// The duration until the next key (end key wraps to first...)
};

void DnfMesh::ExportPSA(FArchive& Ar)
{
	debugf(TEXT("Export PSA..."));
	const INT NumBones = skm_Bones.Num();
	const INT NumAnims = Animations.Num();
	TArray<FNamedBoneBinary> AnimBones(NumBones);
	TArray<AnimInfoBinary> Anims(NumAnims);
	TArray<VQuatAnimKey> Keys;

	INT i, j;
	{
		for (i = 0; i < NumBones; ++i)
		{
			FNamedBoneBinary& B = AnimBones(i);
			CCpjSklBone& RB = skm_Bones(i);
			B.BonePos.Orientation = RB.FinalQuat;
			B.BonePos.Position = RB.FinalPose;
			B.Flags = 0;
			CopyStringToAnsi(B.Name, *RB.name, ARRAY_COUNT(B.Name));
			INT numCh = 0;
			for (j = 0; j < skm_Bones.Num(); ++j)
				if (i != j && skm_Bones(j).parentBone == &RB)
					++numCh;
			B.NumChildren = numCh;
			B.ParentIndex = RB.parentBone ? RB.parentBone->Index : 0;
		}
	}
	{
		INT z, iBase;
		INT TotalFrames = 0;
		VQuatAnimKey* Q;
		FLOAT TTime;
		for (i = 0; i < NumAnims; ++i)
		{
			AnimInfoBinary& A = Anims(i);
			const FAnimSequence& RA = Animations(i);
			const INT NumFrames = RA.m_Frames.Num();
			CopyStringToAnsi(A.Name, *RA.AnimName, ARRAY_COUNT(A.Name));
			CopyStringToAnsi(A.Group, TEXT(""), ARRAY_COUNT(A.Name));
			A.TotalBones = NumBones * NumFrames;
			A.RootInclude = 1;
			A.KeyCompressionStyle = 0;
			A.KeyQuotum = 0;
			A.KeyReduction = 0.f;
			A.TrackTime = FLOAT(NumFrames);
			A.AnimRate = 1.f;
			A.StartBone = 0;
			A.FirstRawFrame = TotalFrames;
			A.NumRawFrames = NumFrames;
			TotalFrames += NumFrames;

			iBase = Keys.Add(A.TotalBones);
			Q = &Keys(iBase);
			for (z = 0; z < NumFrames; ++z)
			{
				const CCpjSeqFrame& F = RA.m_Frames(z);
				TTime = FLOAT(z);
				for (j = 0; j < NumBones; ++j)
				{
					Q->Orientation = F.rotates(j).quat;
					Q->Position = F.translates(j).FinalPose;
					Q->Time = TTime;
					++Q;
				}
			}
		}
	}

	// Serialize everything.
	VChunkHeader Chunk; // Main header.
	appMemzero(&Chunk, sizeof(VChunkHeader));
	CopyStringToAnsi(Chunk.ChunkID, TEXT("ANIMHEAD"), ARRAY_COUNT(Chunk.ChunkID));
	Chunk.TypeFlag = 1999801;
	Ar.Serialize(&Chunk, sizeof(VChunkHeader));

	// Write PSA specific data.
	WriteChunk(Ar, FNamedBoneBinary, AnimBones, TEXT("BONENAMES")); // Bones.
	WriteChunk(Ar, AnimInfoBinary, Anims, TEXT("ANIMINFO")); // Anims.
	WriteChunk(Ar, VQuatAnimKey, Keys, TEXT("ANIMKEYS")); // Raw keys.

	// Unused, but in for compatibility.
	CopyStringToAnsi(Chunk.ChunkID, TEXT("SCALEKEYS"), ARRAY_COUNT(Chunk.ChunkID));
	Chunk.DataCount = 0;
	Chunk.DataSize = 0;
	Ar.Serialize(&Chunk, sizeof(VChunkHeader));
}

// James mesh info.
struct FJSDataHeader
{
	_WORD	NumPolys;
	_WORD	NumVertices;
	_WORD	BogusRot;
	_WORD	BogusFrame;
	DWORD	BogusNormX, BogusNormY, BogusNormZ;
	DWORD	FixScale;
	DWORD	Unused1, Unused2, Unused3;
};

struct FMeshUV
{
	BYTE U;
	BYTE V;

	inline void Set(const FUVMap& UV)
	{
		U = appRound(UV.U * 255.f);
		V = appRound(UV.V * 255.f);
	}
};

// Mesh triangle.
struct FJSMeshTri
{
	_WORD		iVertex[3];		// Vertex indices.
	BYTE		Type;			// James' mesh type.
	BYTE		Color;			// Color for flat and Gouraud shaded.
	FMeshUV		Tex[3];			// Texture UV coordinates.
	BYTE		TextureNum;		// Source texture offset.
	BYTE		Flags;			// Unreal mesh flags (currently unused).
};

void DnfMesh::Export3DD(FArchive& Ar)
{
	debugf(TEXT("Export d_3d..."));

	INT i, j;
	CCpjLodLevel* mLod = NULL;
	if (m_Levels.Num())
		mLod = &m_Levels(m_Levels.Num() - 1);
	const INT NumVerts = mLod ? mLod->vertRelay.Num() : m_Verts.Num();
	const INT NumPolies = mLod ? mLod->triangles.Num() : m_Tris.Num();
	check(m_Tris.Num() == sm_Tris.Num());

	{
		// Write mesh header data.
		FJSDataHeader	JSDataHdr;
		appMemzero(&JSDataHdr, sizeof(FJSDataHeader));
		JSDataHdr.NumVertices = NumVerts;
		JSDataHdr.NumPolys = NumPolies;
		Ar.Serialize(&JSDataHdr, sizeof(FJSDataHeader));
	}
	{
		// Write dummy bits
		BYTE Dummy[12];
		appMemzero(Dummy, sizeof(Dummy));
		Ar.Serialize(&Dummy[0], sizeof(Dummy));
	}

	// Write mesh triangles.
	FJSMeshTri Tri;
	UBOOL bWarnedBounds = FALSE;
	if (mLod)
	{
		for (i = 0; i < NumPolies; i++)
		{
			const CCpjLodTri& GW = mLod->triangles(i);
			const CCpjSrfTri& RW = sm_Tris(GW.srfTriIndex);

			for (j = 0; j < 3; ++j)
			{
				Tri.iVertex[j] = GW.vertIndex[j];
				Tri.Tex[j].Set(m_UV(GW.uvIndex[j]));
			}
			Tri.TextureNum = RW.texIndex;
			if (Tri.TextureNum >= m_Textures.Num())
			{
				if (!bWarnedBounds)
				{
					warnf(TEXT("Texture Index out of bounds %i/%i"), INT(Tri.TextureNum), m_Textures.Num());
					bWarnedBounds = TRUE;
				}
				Tri.TextureNum = 0;
			}
			Tri.Type = RW.GetMTFlags();
			Ar.Serialize(&Tri, sizeof(FJSMeshTri));
		}
	}
	else
	{
		for (i = 0; i < sm_Tris.Num(); i++)
		{
			const CCpjGeoTri& GW = m_Tris(i);
			const CCpjSrfTri& RW = sm_Tris(i);

			for (j = 0; j < 3; ++j)
			{
				Tri.iVertex[j] = GW.edgeRing[j]->tailVertex->iVertex;
				Tri.Tex[j].Set(m_UV(RW.uvIndex[j]));
			}
			Tri.TextureNum = RW.texIndex;
			if (Tri.TextureNum >= m_Textures.Num())
			{
				if (!bWarnedBounds)
				{
					warnf(TEXT("Texture Index out of bounds %i/%i"), INT(Tri.TextureNum), m_Textures.Num());
					bWarnedBounds = TRUE;
				}
				Tri.TextureNum = 0;
			}
			Tri.Type = 0;
			Ar.Serialize(&Tri, sizeof(FJSMeshTri));
		}
	}
}

// James animation info.
struct FJSAnivHeader
{
	_WORD	NumFrames;		// Number of animation frames.
	_WORD	FrameSize;		// Size of one frame of animation.
};

// Packed mesh vertex point for skinned meshes.
#define GET_MESHVERT_DWORD(mv) (*(DWORD*)&(mv))
struct FMeshVert
{
	INT X : 11; INT Y : 11; INT Z : 10;

	// Constructor.
	FMeshVert()
	{}
	FMeshVert(const FVector& In)
		: X(Clamp(appRound(In.X), -1023, 1023)), Y(Clamp(appRound(In.Y), -1023, 1023)), Z(Clamp(appRound(In.Z), -511, 511))
	{}

	// Functions.
	FVector Vector() const
	{
		return FVector(X, Y, Z);
	}
};

void DnfMesh::Export3DA(FArchive& Ar)
{
	debugf(TEXT("Export a_3d..."));

	CCpjLodLevel* mLod = NULL;
	if (m_Levels.Num())
		mLod = &m_Levels(m_Levels.Num() - 1);
	const INT NumVerts = mLod ? mLod->vertRelay.Num() : m_Verts.Num();
	INT i;

	INT NumFrames = 0;
	if (Animations.Num())
	{
		for (i = 0; i < Animations.Num(); ++i)
		{
			NumFrames += Animations(i).m_Frames.Num();
		}
	}
	else
	{
		NumFrames = 1;
	}
	{
		// Write animation header data.
		FJSAnivHeader JSAnivHdr;
		JSAnivHdr.NumFrames = (_WORD)NumFrames;
		JSAnivHdr.FrameSize = (_WORD)(NumVerts * sizeof(FMeshVert));
		Ar.Serialize(&JSAnivHdr, sizeof(FJSAnivHeader));
	}
	{
		// Build complete set of animation frames...
		TArray<FMeshVert> FrameVerts(NumFrames * NumVerts);
		FMeshVert* Out = &FrameVerts(0);
		NWord* relay = mLod ? &mLod->vertRelay(0) : nullptr;
		INT j,z;
		if (Animations.Num())
		{
			for (i = 0; i < Animations.Num(); ++i)
			{
				const INT Count = Animations(i).m_Frames.Num();
				const CCpjSeqFrame* gf = &Animations(i).m_Frames(0);
				for (j = 0; j < Count; ++j)
				{
					const CCpjFrmFrame* Fr = FindFrame(*gf[j].vertFrameName);
					if (!Fr)
					{
						warnf(TEXT("Failed to find frame '%ls'!"), *gf[j].vertFrameName);
					}
					else if (relay)
					{
						const FVector* gPure = &Fr->m_PurePos(0);
						for (z = 0; z < NumVerts; ++z)
						{
							*Out = FMeshVert(PreMulti(gPure[relay[z]]));
							++Out;
						}
					}
					else
					{
						const FVector* gPure = &Fr->m_PurePos(0);
						for (z = 0; z < NumVerts; ++z)
						{
							*Out = FMeshVert(PreMulti(gPure[z]));
							++Out;
						}
					}
				}
			}
		}
		else if (relay)
		{
			const CCpjGeoVert* iv = &m_Verts(0);
			for (i = 0; i < NumVerts; ++i)
			{
				*Out = FMeshVert(PreMulti(iv[relay[i]].refPosition));
			}
		}
		else
		{
			const CCpjGeoVert* iv = &m_Verts(0);
			for (i = 0; i < NumVerts; ++i)
			{
				*Out = FMeshVert(PreMulti(iv[i].refPosition));
			}
		}
		Ar.Serialize(&FrameVerts(0), sizeof(FMeshVert) * FrameVerts.Num());
	}
}
