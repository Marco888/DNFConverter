// DNFConverter.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "CorePrivate.h"

#define DEBUG_SINGLE_MESH 0
#if DEBUG_SINGLE_MESH
#define DEBUG_MESH_NAME TEXT("alien_Adultsnatcher")
#endif

#include "DnfMesh.h"
#include "UnLinker.h"

// Log.
#include "FOutputDeviceFile.h"
FOutputDeviceFile Log;

// Error.
#include "FOutputDeviceAnsiError.h"
FOutputDeviceAnsiError Error;

// Feedback.
#include "FFeedbackContextAnsi.h"
FFeedbackContextAnsi Warn;

#include "FConfigCacheIni.h"

// File manager.
#include "FFileManagerWindows.h"
FFileManagerWindows FileManager;
#include "FMallocAnsi.h"
FMallocAnsi Malloc;

BOOL ExitHandle(DWORD fdwCtrlType)
{
	appRequestExit(0);
	return TRUE;
}

void ConvertModel(const TCHAR* File)
{
	warnf(TEXT("Load: %ls"), File);
	FArchive* Ar = GFileManager->CreateFileReader(File, 0, GWarn);
	if (!Ar)
		return;

	FString FilePath(File);
	{
		TCHAR* Str = &FilePath[0];
		TCHAR* Fin = NULL;
		while (*Str)
		{
			if (*Str == '/' || *Str == '\\')
				Fin = Str + 1;
			++Str;
		}
		if (Fin)
			*Fin = 0;
	}
	LinkerLoad Linker(Ar, *FilePath);
	delete Ar;
}

int _tmain(int argc, _TCHAR* argv[])
{
	INT ErrorLevel = 0;
	GIsStarted = 1;
	SetConsoleCtrlHandler((PHANDLER_ROUTINE)ExitHandle, TRUE);
#ifndef _DEBUG
	try
#endif
	{
		GIsGuarded = 1;

		// Parse command line.
		TCHAR* Ch = GetCommandLineW();
		while (*Ch && *Ch != ' ')
			Ch++;
		while (*Ch == ' ')
			Ch++;

		// Init engine core.
#ifdef __UNIX__
		appInit(Ch, &Malloc, &Log, &Error, &Warn, &FileManager, FConfigCacheIni::Factory);
#elif _WIN32
		appInit(Ch, &Malloc, &Log, &Error, &Warn, &FileManager, FConfigCacheIni::Factory);
#endif
		if (!*Ch)
			GWarn->Log(TEXT("ERROR: Missing input file!"));
		else
		{
			TCHAR FileName[1024];
			while (*Ch)
			{
				TCHAR* fn = FileName;
				if (*Ch == '\"')
				{
					++Ch;
					while (*Ch && *Ch != '\"')
						*fn++ = *Ch++;
					if (*Ch == '\"')
						++Ch;
				}
				else
				{
					while (*Ch && *Ch != ' ')
						*fn++ = *Ch++;
				}
				while (*Ch == ' ')
					++Ch;
				*fn = 0;
				ConvertModel(FileName);
			}
			GWarn->Log(TEXT("All done!"));
		}
		appPreExit();
		GIsGuarded = 0;
	}
	catch (...)
	{
		// Crashed.
		ErrorLevel = 1;
		GIsGuarded = 0;
		Error.HandleError();
	}
	//system("pause");
	appExit();
	return ErrorLevel;
}
