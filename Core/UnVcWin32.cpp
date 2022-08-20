/*=============================================================================
	UnVcWin32.cpp: Visual C++ Windows 32-bit core.
	Copyright 1997-1999 Epic Games, Inc. All Rights Reserved.

	Revision history:
		* Created by Tim Sweeney
=============================================================================*/
#if _MSC_VER

#include <stdio.h>
#include <float.h>
#include <time.h>
#include <io.h>
#include <direct.h>
#include <errno.h>
#include <sys/stat.h>

// Core includes.
#include "CorePrivate.h"

#include <string.h>

/*-----------------------------------------------------------------------------
	Unicode helpers.
-----------------------------------------------------------------------------*/

#if UNICODE

ANSICHAR* winToANSI( ANSICHAR* ACh, const TCHAR* InUCh, INT Count )
{
	WideCharToMultiByte( CP_ACP, 0, InUCh, -1, ACh, Count, NULL, NULL );
	return ACh;
}
ANSICHAR* winToOEM( ANSICHAR* ACh, const TCHAR* InUCh, INT Count )
{
	WideCharToMultiByte( CP_OEMCP, 0, InUCh, -1, ACh, Count, NULL, NULL );
	return ACh;
}
INT winGetSizeANSI( const TCHAR* InUCh )
{
	return WideCharToMultiByte( CP_ACP, 0, InUCh, -1, NULL, 0, NULL, NULL );
}
TCHAR* winToUNICODE( TCHAR* UCh, const ANSICHAR* InACh, INT Count )
{
	MultiByteToWideChar( CP_ACP, 0, InACh, -1, UCh, Count );
	return UCh;
}
INT winGetSizeUNICODE( const ANSICHAR* InACh )
{
	return MultiByteToWideChar( CP_ACP, 0, InACh, -1, NULL, 0 );
}

UNICHAR* winAnsiToTCHAR(char* str)
{
	INT iLength = winGetSizeUNICODE(str);
	UNICHAR* pBuffer = new TCHAR[iLength];
	appStrcpy(pBuffer,TEXT(""));
	return winToUNICODE(pBuffer,str,iLength);
}

#endif	// UNICODE

/*-----------------------------------------------------------------------------
	FOutputDeviceWindowsError.
-----------------------------------------------------------------------------*/

//
// Immediate exit.
//
void appRequestExit( UBOOL Force )
{
	debugf( TEXT("appRequestExit(%i)"), Force );
	if( Force )
	{
		// Force immediate exit. Dangerous because config code isn't flushed, etc.
		ExitProcess( 1 );
	}
	else
	{
		// Tell the platform specific code we want to exit cleanly from the main loop.
		PostQuitMessage( 0 );
		GIsRequestingExit = 1;
	}
}

//
// Get system error.
//
const TCHAR* appGetSystemErrorMessage( INT Error )
{
	static TCHAR Msg[1024];
	*Msg = 0;
	if( Error==0 )
		Error = GetLastError();
#if UNICODE
	if( !GUnicodeOS )
	{
		ANSICHAR ACh[1024];
		FormatMessageA( FORMAT_MESSAGE_FROM_SYSTEM, NULL, Error, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), ACh, 1024, NULL );
		appStrcpy( Msg, ANSI_TO_TCHAR(ACh) );
	}
	else
#endif
	{
		FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, Error, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), Msg, 1024, NULL );
	}
	if( appStrchr(Msg,'\r') )
		*appStrchr(Msg,'\r')=0;
	if( appStrchr(Msg,'\n') )
		*appStrchr(Msg,'\n')=0;
	return Msg;
}

/*-----------------------------------------------------------------------------
	Clipboard.
-----------------------------------------------------------------------------*/

//
// Copy text to clipboard.
//
void appClipboardCopy( const TCHAR* Str )
{
	if( OpenClipboard(GetActiveWindow()) )
	{
		verify(EmptyClipboard());
#if UNICODE
		HGLOBAL GlobalMem;
		if( GUnicode && !GUnicodeOS )
		{
			INT Count = WideCharToMultiByte(CP_ACP,0,Str,-1,NULL,0,NULL,NULL);
			GlobalMem = GlobalAlloc( GMEM_DDESHARE | GMEM_MOVEABLE, Count+1 );
			check(GlobalMem);
			ANSICHAR* Data = (ANSICHAR*) GlobalLock( GlobalMem );
			WideCharToMultiByte(CP_ACP,0,Str,-1,Data,Count,NULL,NULL);
			Data[Count] = 0;
			GlobalUnlock( GlobalMem );
			if( SetClipboardData( CF_TEXT, GlobalMem ) == NULL )
				appErrorf(TEXT("SetClipboardData(A) failed with error code %i"), GetLastError() );
		}
		else
#endif
		{
			GlobalMem = GlobalAlloc( GMEM_DDESHARE | GMEM_MOVEABLE, sizeof(TCHAR)*(appStrlen(Str)+1) );
			check(GlobalMem);
			TCHAR* Data = (TCHAR*) GlobalLock( GlobalMem );
			appStrcpy( Data, Str );
			GlobalUnlock( GlobalMem );
			if( SetClipboardData( GUnicode ? CF_UNICODETEXT : CF_TEXT, GlobalMem ) == NULL )
				appErrorf(TEXT("SetClipboardData(%s) failed with error code %i"), GUnicode ? TEXT("W") : TEXT("A"), GetLastError() );
		}
		verify(CloseClipboard());
	}
}

//
// Paste text from clipboard.
//
FString appClipboardPaste()
{
	FString Result;
	if( OpenClipboard(GetActiveWindow()) )
	{
		HGLOBAL GlobalMem = NULL;
		UBOOL Unicode = 0;
		if( GUnicode && GUnicodeOS )
		{
			GlobalMem = GetClipboardData( CF_UNICODETEXT );
			Unicode = 1;
		}
		if( !GlobalMem )
		{
			GlobalMem = GetClipboardData( CF_TEXT );
			Unicode = 0;
		}
		if( !GlobalMem )
			Result = TEXT("");
		else
		{
			void* Data = GlobalLock( GlobalMem );
			check( Data );	
			if( Unicode )
				Result = (TCHAR*) Data;
			else
			{
				ANSICHAR* ACh = (ANSICHAR*) Data;
				INT i;
				for( i=0; ACh[i]; i++ );
				TArray<TCHAR> Ch(i+1);
				for( i=0; i<Ch.Num(); i++ )
					Ch(i)=FromAnsi(ACh[i]);
				Result = &Ch(0);
			}
			GlobalUnlock( GlobalMem );
		}
		verify(CloseClipboard());
	}
	else Result=TEXT("");

	return Result;
}

/*-----------------------------------------------------------------------------
	DLLs.
-----------------------------------------------------------------------------*/

void* appGetDllHandle( const TCHAR* Filename )
{
	check(Filename);	
	return TCHAR_CALL_OS(LoadLibraryW(Filename),LoadLibraryA(TCHAR_TO_ANSI(Filename)));
}

//// Free a DLL.
//
void appFreeDllHandle( void* DllHandle )
{
	check(DllHandle);
	FreeLibrary( (HMODULE)DllHandle );
}

//
// Lookup the address of a DLL function.
//
void* appGetDllExport( void* DllHandle, const TCHAR* ProcName )
{
	check(DllHandle);
	check(ProcName);
	return (void*)GetProcAddress( (HMODULE)DllHandle, TCHAR_TO_ANSI(ProcName) );
}

const void appDebugMessagef( const TCHAR* Fmt, ... )
{
	TCHAR TempStr[4096]=TEXT("");
	GET_VARARGS( TempStr, ARRAY_COUNT(TempStr), Fmt, Fmt );
	MessageBox(NULL,TempStr,TEXT("appDebugMessagef"),MB_OK);
}

// Type - dicrates the type of dialog we're displaying
VARARG_BODY( const UBOOL, appMsgf, const TCHAR*, VARARG_EXTRA(INT Type) )
{
	TCHAR TempStr[4096]=TEXT("");
	GET_VARARGS( TempStr, ARRAY_COUNT(TempStr), Fmt, Fmt );
	switch( Type )
	{
		case 1:
			return MessageBox( NULL, TempStr, TEXT("Message"), MB_YESNO ) == IDYES;
			break;
		case 2:
			return MessageBox( NULL, TempStr, TEXT("Message"), MB_OKCANCEL ) == IDOK;
			break;
		default:
			MessageBox( NULL, TempStr, TEXT("Message"), MB_OK );
			break;
	}
	return 1;
}

const void appGetLastError( void )
{
	TCHAR TempStr[4096]=TEXT("");
	appSprintf( TempStr, TEXT("GetLastError : %d\n\n%s"),
		GetLastError(), appGetSystemErrorMessage() );
	MessageBox( NULL, TempStr, TEXT("System Error"), MB_OK );
}

//
// Break the debugger.
//
void appDebugBreak()
{
	::DebugBreak();
}

//
// appIsDebuggerPresent - are we running under a debugger?
//
UBOOL appIsDebuggerPresent()
{
	UBOOL Result = 0;
    OSVERSIONINFO WinVersion;
    WinVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

    // IsDebuggerPresent doesn't exist under 95, so we will only test for it under NT+
    if( GetVersionEx(&WinVersion) && WinVersion.dwPlatformId == VER_PLATFORM_WIN32_NT)
    {
        HINSTANCE KernLib =  LoadLibraryEx( TEXT("kernel32.dll"), NULL, 0);
        if( KernLib )
        {
            FARPROC lIsDebuggerPresent = GetProcAddress( KernLib, "IsDebuggerPresent" );
            if( lIsDebuggerPresent && lIsDebuggerPresent() )
                Result = 1;

            FreeLibrary( KernLib );
        }
    }
	return Result;
}



/*-----------------------------------------------------------------------------
	Timing.
-----------------------------------------------------------------------------*/

//
// Get time in seconds. Origin is arbitrary.
//
#if !DEFINED_appSeconds
DOUBLE appSeconds()
{
	static DWORD  InitialTime = timeGetTime();
	static DOUBLE TimeCounter = 0.0;

	// Accumulate difference to prevent wraparound.
	DWORD NewTime = timeGetTime();
	TimeCounter += (NewTime - InitialTime) * (1./1000.);
	InitialTime = NewTime;

	return TimeCounter;
}
#endif

//
// Return number of CPU cycles passed. Origin is arbitrary.
//
#if !DEFINED_appCycles
DWORD appCycles()
{
	return appSeconds();
}
#endif

//
// Sleep this thread for Seconds, 0.0 means release the current
// timeslice to let other threads get some attention.
//
void appSleep( FLOAT Seconds )
{
	Sleep( (DWORD)(Seconds * 1000.0) );
}

//
// Return the system time.
//
void appSystemTime( INT& Year, INT& Month, INT& DayOfWeek, INT& Day, INT& Hour, INT& Min, INT& Sec, INT& MSec )
{
	SYSTEMTIME st;
	GetLocalTime( &st );

	Year		= st.wYear;
	Month		= st.wMonth;
	DayOfWeek	= st.wDayOfWeek;
	Day			= st.wDay;
	Hour		= st.wHour;
	Min			= st.wMinute;
	Sec			= st.wSecond;
	MSec		= st.wMilliseconds;
}

//
//	Return the system time as a 64-bit number.
//

SQWORD appSystemTime64()
{
	SYSTEMTIME	SystemTime;
	FILETIME	FileTime;

	GetLocalTime(&SystemTime);
	verify(SystemTimeToFileTime(&SystemTime,&FileTime));

	return *(SQWORD*)&FileTime;
}

//
// Return seconds counter.
//
DOUBLE appSecondsSlow()
{
	static INT LastTickCount=0;
	static DOUBLE TickCount=0.0;
	INT ThisTickCount = GetTickCount();
	TickCount += (ThisTickCount-LastTickCount) / 1000.0;
	LastTickCount = ThisTickCount;
	return TickCount;
}

/*-----------------------------------------------------------------------------
	Link functions.
-----------------------------------------------------------------------------*/

//
// Launch a uniform resource locator (i.e. http://www.epicgames.com/unreal).
// This is expected to return immediately as the URL is launched by another
// task.
//
void appLaunchURL( const TCHAR* URL, const TCHAR* Parms, FString* Error )
{
}

void *appCreateProc( const TCHAR* URL, const TCHAR* Parms )
{
	return nullptr;
}

UBOOL appGetProcReturnCode( void* ProcHandle, INT* ReturnCode )
{
	return FALSE;
}

/*-----------------------------------------------------------------------------
	File finding.
-----------------------------------------------------------------------------*/

//
// Clean out the file cache.
//
static INT GetFileAgeDays( const TCHAR* Filename )
{
	struct _stat Buf;
	INT Result = 0;
#if UNICODE
	if( GUnicodeOS )
	{
		Result = _wstat(Filename,&Buf);
	}
	else
#endif
	{
		Result = _stat(TCHAR_TO_ANSI(Filename),&Buf);
	}
	if( Result==0 )
	{
		time_t CurrentTime, FileTime;
		FileTime = Buf.st_mtime;
		time( &CurrentTime );
		DOUBLE DiffSeconds = difftime( CurrentTime, FileTime );
		return DiffSeconds / 60.0 / 60.0 / 24.0;
	}
	return 0;
}

/*-----------------------------------------------------------------------------
	Command line.
-----------------------------------------------------------------------------*/

// Get startup directory.
const TCHAR* appBaseDir()
{
	static TCHAR Result[MAX_PATH]=TEXT("");
	if (!Result[0])
	{
		// Get directory this executable was launched from.
		GetModuleFileName(NULL, Result, MAX_PATH);

		TCHAR* f = NULL;
		TCHAR* S = Result;
		while (*S)
		{
			if(*S=='\\' || *S=='/')
				f = S + 1;
			++S;
		}
		if (f)
			*f = 0;
	}
	return Result;
}

// Get computer name.
const TCHAR* appComputerName()
{
	static TCHAR Result[256]=TEXT("");
	if( !Result[0] )
	{
		DWORD Size=ARRAY_COUNT(Result);
#if UNICODE
		if( GUnicode && !GUnicodeOS )
		{
			ANSICHAR ACh[ARRAY_COUNT(Result)];
			GetComputerNameA( ACh, &Size );
			MultiByteToWideChar( CP_ACP, 0, ACh, -1, Result, ARRAY_COUNT(Result) );
		}
		else
#endif
		{
			GetComputerName( Result, &Size );
		}
		TCHAR *c, *d;
		for( c=Result, d=Result; *c!=0; c++ )
			if( appIsAlnum(*c) )
				*d++ = *c;
		*d++ = 0;
	}
	return Result;
}

// Get user name.
const TCHAR* appUserName()
{
	static TCHAR Result[256]=TEXT("");
	if( !Result[0] )
	{
		DWORD Size=ARRAY_COUNT(Result);
#if UNICODE
		if( GUnicode && !GUnicodeOS )
		{
			ANSICHAR ACh[ARRAY_COUNT(Result)];
			GetUserNameA( ACh, &Size );
			MultiByteToWideChar( CP_ACP, 0, ACh, -1, Result, ARRAY_COUNT(Result) );
		}
		else
#endif
		{
			GetUserName( Result, &Size );
		}
		TCHAR *c, *d;
		for( c=Result, d=Result; *c!=0; c++ )
			if( appIsAlnum(*c) )
				*d++ = *c;
		*d++ = 0;
	}
	return Result;
}

/*-----------------------------------------------------------------------------
	App init/exit.
-----------------------------------------------------------------------------*/

//
// Platform specific initialization.
//
static void DoCPUID( int i, DWORD *A, DWORD *B, DWORD *C, DWORD *D )
{
#if ASM
 	__asm
	{			
		mov eax,[i]
		_emit 0x0f
		_emit 0xa2

		mov edi,[A]
		mov [edi],eax

		mov edi,[B]
		mov [edi],ebx

		mov edi,[C]
		mov [edi],ecx

		mov edi,[D]
		mov [edi],edx

		mov eax,0
		mov ebx,0
		mov ecx,0
		mov edx,0
		mov esi,0
		mov edi,0
	}
#else
	*A=*B=*C=*D=0;
#endif
}

/** Original C- Runtime pure virtual call handler that is being called in the (highly likely) case of a double fault */
_purecall_handler DefaultPureCallHandler;

/**
 * Our own pure virtual function call handler, set by appPlatformPreInit. Falls back
 * to using the default C- Runtime handler in case of double faulting.
 */
static void PureCallHandler()
{
	static int AlreadyCalled = 0;
	if( AlreadyCalled )
	{
		// Call system handler if we're double faulting.
		if( DefaultPureCallHandler )
			DefaultPureCallHandler();
	}
	else
	{
		AlreadyCalled = 1;
		if( appIsDebuggerPresent() )
		{
			// Break debugger if attached instead of calling appErrorf as the latter could cause a double fault which then would simply terminate the app.
			appDebugBreak();
		}
		else
		{
			if( GIsRunning )
			{
				appMsgf( 0, TEXT("Pure virtual function being called while application was running (GIsRunning == 1).") );
			}
			appErrorf(TEXT("Pure virtual function being called") );
		}
	}
}

void appPlatformPreInit()
{
	// Check Windows version.
	DWORD dwPlatformId, dwMajorVersion, dwMinorVersion, dwBuildNumber;
#if UNICODE
	if (GUnicode && !GUnicodeOS)
	{
		OSVERSIONINFOA Version;
		Version.dwOSVersionInfoSize = sizeof(OSVERSIONINFOA);
		GetVersionExA(&Version);
		dwPlatformId = Version.dwPlatformId;
		dwMajorVersion = Version.dwMajorVersion;
		dwMinorVersion = Version.dwMinorVersion;
		dwBuildNumber = Version.dwBuildNumber;
	}
	else
#endif
	{
		OSVERSIONINFO Version;
		Version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
		GetVersionEx(&Version);
		dwPlatformId = Version.dwPlatformId;
		dwMajorVersion = Version.dwMajorVersion;
		dwMinorVersion = Version.dwMinorVersion;
		dwBuildNumber = Version.dwBuildNumber;
	}
	if (dwPlatformId == VER_PLATFORM_WIN32_NT)
	{
		debugf(NAME_Init, TEXT("Detected: Microsoft Windows NT %u.%u (Build: %u)"), dwMajorVersion, dwMinorVersion, dwBuildNumber);
		GUnicodeOS = 1;
	}

	DefaultPureCallHandler = _set_purecall_handler( PureCallHandler );
}

#ifndef _DEBUG
#pragma optimize( "g", off ) // Fix CPU brand string mangling in release mode
#endif
void appPlatformInit()
{
	// Randomize.
	srand( (unsigned)time( NULL ) );

	// Get CPU info.
	SYSTEM_INFO SI;
	GetSystemInfo(&SI);
	GPageSize = SI.dwPageSize;
	check(!(GPageSize&(GPageSize-1)));
	debugf( NAME_Init, TEXT("CPU Page size=%i, Processors=%i"), SI.dwPageSize, SI.dwNumberOfProcessors );

	// Check processor version with CPUID.
	DWORD A=0, B=0, C=0, D=0;
	DoCPUID(0,&A,&B,&C,&D);
	TCHAR Brand[13];
	Brand[ 0] = (ANSICHAR)(B);
	Brand[ 1] = (ANSICHAR)(B>>8);
	Brand[ 2] = (ANSICHAR)(B>>16);
	Brand[ 3] = (ANSICHAR)(B>>24);
	Brand[ 4] = (ANSICHAR)(D);
	Brand[ 5] = (ANSICHAR)(D>>8);
	Brand[ 6] = (ANSICHAR)(D>>16);
	Brand[ 7] = (ANSICHAR)(D>>24);
	Brand[ 8] = (ANSICHAR)(C);
	Brand[ 9] = (ANSICHAR)(C>>8);
	Brand[10] = (ANSICHAR)(C>>16);
	Brand[11] = (ANSICHAR)(C>>24);
	Brand[12] = (ANSICHAR)(0);
	DoCPUID( 1, &A, &B, &C, &D );
	if( !(D & 0x02000000) )
		appErrorf(TEXT("Engine requires processor with SSE support"));

	// Print features.
	debugf( NAME_Init, TEXT("CPU Detected: %s"), Brand );

	// CPU speed.
	GSecondsPerCycle = 0.001f;

	debugf( NAME_Init, TEXT("CPU Speed=%f MHz"), 0.000001 / GSecondsPerCycle );

	// Get memory.
	MEMORYSTATUS M;
	GlobalMemoryStatus(&M);
	debugf( NAME_Init, TEXT("Memory total: Phys=%iK Pagef=%iK Virt=%iK"), M.dwTotalPhys/1024, M.dwTotalPageFile/1024, M.dwTotalVirtual/1024 );	
}
#ifndef _DEBUG
#pragma optimize("", on)
#endif

#endif
/*-----------------------------------------------------------------------------
	The End.
-----------------------------------------------------------------------------*/

