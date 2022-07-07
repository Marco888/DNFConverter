/*=============================================================================
	UnMisc.cpp: Various core platform-independent functions.
	Copyright 1997-1999 Epic Games, Inc. All Rights Reserved.

	Revision history:
		* Created by Tim Sweeney
=============================================================================*/

// Core includes.
#include "CorePrivate.h"

// For FConfigFile in appInit.
#include "FConfigCacheIni.h"

FArchive& operator<<(FArchive& Ar, FCompactIndex& I)
{
	if (!Ar.IsLoading() && !Ar.IsSaving())
	{
		Ar << I.Value;
	}
	else
	{
		INT   Original = I.Value;
		DWORD V = Abs(I.Value);
		BYTE  B0 = ((I.Value >= 0) ? 0 : 0x80) + ((V < 0x40) ? V : ((V & 0x3f) + 0x40));
		I.Value = 0;
		Ar << B0;
		if (B0 & 0x40)
		{
			V >>= 6;
			BYTE B1 = (V < 0x80) ? V : ((V & 0x7f) + 0x80);
			Ar << B1;
			if (B1 & 0x80)
			{
				V >>= 7;
				BYTE B2 = (V < 0x80) ? V : ((V & 0x7f) + 0x80);
				Ar << B2;
				if (B2 & 0x80)
				{
					V >>= 7;
					BYTE B3 = (V < 0x80) ? V : ((V & 0x7f) + 0x80);
					Ar << B3;
					if (B3 & 0x80)
					{
						V >>= 7;
						BYTE B4 = V;
						Ar << B4;
						I.Value = B4;
					}
					I.Value = (I.Value << 7) + (B3 & 0x7f);
				}
				I.Value = (I.Value << 7) + (B2 & 0x7f);
			}
			I.Value = (I.Value << 7) + (B1 & 0x7f);
		}
		I.Value = (I.Value << 6) + (B0 & 0x3f);
		if (B0 & 0x80)
			I.Value = -I.Value;
		if (Ar.IsSaving() && I.Value != Original)
			appErrorf(TEXT("Mismatch: %08X %08X"), I.Value, Original);
	}
	return Ar;
}

/*-----------------------------------------------------------------------------
	FOutputDevice implementation.
-----------------------------------------------------------------------------*/

void FOutputDevice::Log( EName Event, const TCHAR* Str )
{
	if( !FName::SafeSuppressed(Event) )
		Serialize( Str, Event );
}
void FOutputDevice::Log( const TCHAR* Str )
{
	if( !FName::SafeSuppressed(NAME_Log) )
		Serialize( Str, NAME_Log );
}
void FOutputDevice::Log( const FString& S )
{
	if( !FName::SafeSuppressed(NAME_Log) )
		Serialize( *S, NAME_Log );
}
void FOutputDevice::Log( enum EName Type, const FString& S )
{
	if( !FName::SafeSuppressed(Type) )
		Serialize( *S, Type );
}
VARARG_BODY( void, FOutputDevice::Logf, const TCHAR*, VARARG_EXTRA(enum EName Event) )
{
	if( !FName::SafeSuppressed(Event) )
	{
		INT		BufferSize	= 1024;
		TCHAR*	Buffer		= NULL;
		INT		Result		= -1;

		while(Result == -1)
		{
			Buffer = (TCHAR*) appSystemRealloc( Buffer, BufferSize * sizeof(TCHAR) );
			GET_VARARGS_RESULT(Buffer,BufferSize-1,Fmt,Fmt,Result);
			BufferSize *= 2;
		};
		Buffer[Result] = 0;
		Serialize( Buffer, Event );

		appSystemFree( Buffer );
	}
}
VARARG_BODY( void, FOutputDevice::Logf, const TCHAR*, VARARG_NONE )
{
	// We need to use malloc here directly as GMalloc might not be safe.	
	if( !FName::SafeSuppressed(NAME_Log) )
	{
		INT		BufferSize	= 1024;
		TCHAR*	Buffer		= NULL;
		INT		Result		= -1;

		while(Result == -1)
		{
			Buffer = (TCHAR*) appSystemRealloc( Buffer, BufferSize * sizeof(TCHAR) );
			GET_VARARGS_RESULT(Buffer,BufferSize-1,Fmt,Fmt,Result);
			BufferSize *= 2;
		};
		Buffer[Result] = 0;
		Serialize( Buffer, NAME_Log );

		appSystemFree( Buffer );
	}
}

/*-----------------------------------------------------------------------------
	FArray implementation.
-----------------------------------------------------------------------------*/

void FArray::Realloc( INT ElementSize )
{
	// Avoid calling appRealloc( NULL, 0 ) as ANSI C mandates returning a valid pointer which is not what we want.
	if( Data || ArrayMax )
		Data = appRealloc( Data, ArrayMax*ElementSize );
}

void FArray::Remove( INT Index, INT Count, INT ElementSize )
{
	if( Count )
	{
		appMemmove
		(
			(BYTE*)Data + (Index      ) * ElementSize,
			(BYTE*)Data + (Index+Count) * ElementSize,
			(ArrayNum - Index - Count ) * ElementSize
		);
		ArrayNum -= Count;
		if
		(	(3*ArrayNum<2*ArrayMax || (ArrayMax-ArrayNum)*ElementSize>=16384)
		&&	(ArrayMax-ArrayNum>64 || ArrayNum==0) )
		{
			ArrayMax = ArrayNum;
			Realloc( ElementSize );
		}
	}
	checkSlow(ArrayNum>=0);
	checkSlow(ArrayMax>=ArrayNum);
}

/*-----------------------------------------------------------------------------
	FString implementation.
-----------------------------------------------------------------------------*/

FString FString::Chr( TCHAR Ch )
{
	TCHAR Temp[2]={Ch,0};
	return FString(Temp);
}

FString FString::LeftPad( INT ChCount )
{
	INT Pad = ChCount - Len();
	if( Pad > 0 )
	{
		TCHAR* Ch = (TCHAR*)appAlloca((Pad+1)*sizeof(TCHAR));
		INT i;
		for( i=0; i<Pad; i++ )
			Ch[i] = ' ';
		Ch[i] = 0;
		return FString(Ch) + *this;
	}
	else return *this;
}
FString FString::RightPad( INT ChCount )
{
	INT Pad = ChCount - Len();
	if( Pad > 0 )
	{
		TCHAR* Ch = (TCHAR*)appAlloca((Pad+1)*sizeof(TCHAR));
		INT i;
		for( i=0; i<Pad; i++ )
			Ch[i] = ' ';
		Ch[i] = 0;
		return *this + FString(Ch);
	}
	else return *this;
}

UBOOL FString::IsNumeric()
{
	if ( Len() == 0 )
		return 0;

	TCHAR C = (*this)(0);
	
	if( C == '-' || C =='.' || appIsDigit( C ) )
	{
		UBOOL HasDot = (C == '.');

		for( INT i=1; i<Len(); i++ )
		{
			C = (*this)(i);

			if( C == '.' )
			{
				if( HasDot )
				{
					return 0;
				}
				else
				{
					HasDot = 1;
				}
			}
			else if( !appIsDigit(C) )
			{
				return 0;
			}
		}

		return 1;
	}
	else
	{
		return 0;
	}
}

VARARG_BODY( FString, FString::Printf, const TCHAR*, VARARG_NONE )
{
	INT		BufferSize	= 1024;
	TCHAR*	Buffer		= NULL;
	INT		Result		= -1;

	while(Result == -1)
	{
		Buffer = (TCHAR*) appRealloc( Buffer, BufferSize * sizeof(TCHAR) );
		GET_VARARGS_RESULT(Buffer,BufferSize-1,Fmt,Fmt,Result);
		BufferSize *= 2;
	};
	Buffer[Result] = 0;

	FString ResultString(Buffer);
	appFree( Buffer );

	return ResultString;
}
FArchive& operator<<( FArchive& Ar, FString& A )
{
	A.CountBytes(Ar);
	INT SaveNum = appIsPureAnsi(*A) ? A.Num() : -A.Num();
	Ar << AR_INDEX(SaveNum);
	if (Ar.IsLoading())
	{
		A.ArrayMax = A.ArrayNum = Abs(SaveNum);
		A.Realloc(sizeof(TCHAR));
		if (SaveNum >= 0)
			for (INT i = 0; i < A.Num(); i++)
			{
				ANSICHAR ACh; Ar << *(BYTE*)&ACh; A(i) = FromAnsi(ACh);
			}
		else
			for (INT i = 0; i < A.Num(); i++)
			{
				UNICHAR UCh; Ar << UCh; A(i) = FromUnicode(UCh);
			}
		if (Ar.IsLoading() && A.Num() == 1)
			A.Empty();
	}
	else
	{
		if (SaveNum >= 0)
			for (INT i = 0; i < A.Num(); i++)
			{
				ANSICHAR ACh = ToAnsi(A(i)); Ar << *(BYTE*)&ACh;
			}
		else
			for (INT i = 0; i < A.Num(); i++)
			{
				UNICHAR UCh = ToUnicode(A(i)); Ar << UCh;
			}
	}
	return Ar;
}

/*-----------------------------------------------------------------------------
	String functions.
-----------------------------------------------------------------------------*/

//
// Returns whether the string is pure ANSI.
//
UBOOL appIsPureAnsi( const TCHAR* Str )
{
#if UNICODE
	for( ; *Str; Str++ )
		if( *Str<0 || *Str>0xff )
			return 0;
#endif
	return 1;
}

//
// Failed assertion handler.
//warning: May be called at library startup time.
//
void VARARGS appFailAssert( const ANSICHAR* Expr, const ANSICHAR* File, INT Line )
{
	GWarn->Logf( TEXT("Assertion failed: %s [File:%s] [Line: %i]"), ANSI_TO_TCHAR(Expr), ANSI_TO_TCHAR(File), Line );
}

//
// Gets the extension of a file, such as "PCX".  Returns NULL if none.
// string if there's no extension.
//
const TCHAR* appFExt( const TCHAR* fname )
{
	if( appStrstr(fname,TEXT(":")) )
		fname = appStrstr(fname,TEXT(":"))+1;

	while( appStrstr(fname,TEXT("/")) )
		fname = appStrstr(fname,TEXT("/"))+1;

	while( appStrstr(fname,TEXT(".")) )
		fname = appStrstr(fname,TEXT("."))+1;

	return fname;
}

//
// Convert an integer to a string.
//
TCHAR* appItoa( const INT Num )
{
#if _MSC_VER
#if UNICODE
	static TCHAR Buffer[20];
	appMemzero( Buffer, 20*sizeof(TCHAR) );
	return _itow( Num, Buffer, 10 );
#else
	static char Buffer[20];
	appMemzero( Buffer, 20*sizeof(char) );
	return _itoa( Num, Buffer, 10 );
#endif
#else
	appErrorf(TEXT("Not implemented"));
	return TEXT("Not implemented");
#endif
}

//
// Find string in string, case insensitive, requires non-alphanumeric lead-in.
//
const TCHAR* appStrfind( const TCHAR* Str, const TCHAR* Find )
{
	UBOOL Alnum  = 0;
	TCHAR f      = (*Find<'a' || *Find>'z') ? (*Find) : (*Find+'A'-'a');
	INT   Length = appStrlen(Find++)-1;
	TCHAR c      = *Str++;
	while( c )
	{
		if( c>='a' && c<='z' )
			c += 'A'-'a';
		if( !Alnum && c==f && !appStrnicmp(Str,Find,Length) )
			return Str-1;
		Alnum = (c>='A' && c<='Z') || (c>='0' && c<='9');
		c = *Str++;
	}
	return NULL;
}

//
// Returns a certain number of spaces.
// Only one return value is valid at a time.
//
const TCHAR* appSpc( INT Num )
{
	static TCHAR Spacing[256];
	static INT OldNum=-1;
	if( Num != OldNum )
	{
		for( OldNum=0; OldNum<Num; OldNum++ )
			Spacing[OldNum] = ' ';
		Spacing[Num] = 0;
	}
	return Spacing;
}

// 
// Trim spaces from an asciiz string by zeroing them.
//
void appTrimSpaces( ANSICHAR* String )
{		
	// Find 0 terminator.
	INT t=0;
	while( (String[t]!=0 ) && (t< 1024) ) t++;
	if (t>0) t--;
	// Zero trailing spaces.
	while( (String[t]==32) && (t>0) )
	{
		String[t]=0;
		t--;
	}
}


/*-----------------------------------------------------------------------------
	Memory functions.
-----------------------------------------------------------------------------*/

//
// Memory functions.
//
void appMemswap( void* Ptr1, void* Ptr2, DWORD Size )
{
	void* Temp = appAlloca(Size);
	appMemcpy( Temp, Ptr1, Size );
	appMemcpy( Ptr1, Ptr2, Size );
	appMemcpy( Ptr2, Temp, Size );
}

/*-----------------------------------------------------------------------------
	CRC functions.
-----------------------------------------------------------------------------*/

// CRC 32 polynomial.
#define CRC32_POLY 0x04c11db7
DWORD GCRCTable[256];

//
// CRC32 computer based on CRC32_POLY.
//
DWORD appMemCrc( const void* InData, INT Length, DWORD CRC )
{
	BYTE* Data = (BYTE*)InData;
	CRC = ~CRC;
	for( INT i=0; i<Length; i++ )
		CRC = (CRC << 8) ^ GCRCTable[(CRC >> 24) ^ Data[i]];
	return ~CRC;
}

//
// String CRC.
//
DWORD appStrCrc( const TCHAR* Data )
{
	INT Length = appStrlen( Data );
	DWORD CRC = 0xFFFFFFFF;
	for( INT i=0; i<Length; i++ )
	{
		TCHAR C   = Data[i];
		INT   CL  = (C&255);
		CRC       = (CRC << 8) ^ GCRCTable[(CRC >> 24) ^ CL];;
#if UNICODE
		INT   CH  = (C>>8)&255;
		CRC       = (CRC << 8) ^ GCRCTable[(CRC >> 24) ^ CH];;
#endif
	}
	return ~CRC;
}

//
// String CRC, case insensitive.
//
DWORD appStrCrcCaps( const TCHAR* Data )
{
	INT Length = appStrlen( Data );
	DWORD CRC = 0xFFFFFFFF;
	for( INT i=0; i<Length; i++ )
	{
		TCHAR C   = appToUpper(Data[i]);
		INT   CL  = (C&255);
		CRC       = (CRC << 8) ^ GCRCTable[(CRC >> 24) ^ CL];
#if UNICODE
		INT   CH  = (C>>8)&255;
		CRC       = (CRC << 8) ^ GCRCTable[(CRC >> 24) ^ CH];
#endif
	}
	return ~CRC;
}

//
// Returns smallest N such that (1<<N)>=Arg.
// Note: appCeilLogTwo(0)=0 because (1<<0)=1 >= 0.
//
static BYTE GLogs[257];
BYTE appCeilLogTwo( DWORD Arg )
{
	if( --Arg == MAXDWORD )
		return 0;
	BYTE Shift = Arg<=0x10000 ? (Arg<=0x100?0:8) : (Arg<=0x1000000?16:24);
	return Shift + GLogs[Arg>>Shift];
}

/*-----------------------------------------------------------------------------
	MD5 functions, adapted from MD5 RFC by Brandon Reinhart
-----------------------------------------------------------------------------*/

//
// Constants for MD5 Transform.
//

enum {S11=7};
enum {S12=12};
enum {S13=17};
enum {S14=22};
enum {S21=5};
enum {S22=9};
enum {S23=14};
enum {S24=20};
enum {S31=4};
enum {S32=11};
enum {S33=16};
enum {S34=23};
enum {S41=6};
enum {S42=10};
enum {S43=15};
enum {S44=21};

static BYTE PADDING[64] = {
	0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0
};

//
// Basic MD5 transformations.
//
#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

//
// Rotates X left N bits.
//
#define ROTLEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))

//
// Rounds 1, 2, 3, and 4 MD5 transformations.
// Rotation is separate from addition to prevent recomputation.
//
#define FF(a, b, c, d, x, s, ac) { \
	(a) += F ((b), (c), (d)) + (x) + (DWORD)(ac); \
	(a) = ROTLEFT ((a), (s)); \
	(a) += (b); \
}

#define GG(a, b, c, d, x, s, ac) { \
	(a) += G ((b), (c), (d)) + (x) + (DWORD)(ac); \
	(a) = ROTLEFT ((a), (s)); \
	(a) += (b); \
}

#define HH(a, b, c, d, x, s, ac) { \
	(a) += H ((b), (c), (d)) + (x) + (DWORD)(ac); \
	(a) = ROTLEFT ((a), (s)); \
	(a) += (b); \
}

#define II(a, b, c, d, x, s, ac) { \
	(a) += I ((b), (c), (d)) + (x) + (DWORD)(ac); \
	(a) = ROTLEFT ((a), (s)); \
	(a) += (b); \
}

//
// MD5 initialization.  Begins an MD5 operation, writing a new context.
//
void appMD5Init( FMD5Context* context )
{
	context->count[0] = context->count[1] = 0;
	// Load magic initialization constants.
	context->state[0] = 0x67452301;
	context->state[1] = 0xefcdab89;
	context->state[2] = 0x98badcfe;
	context->state[3] = 0x10325476;
}

//
// MD5 block update operation.  Continues an MD5 message-digest operation,
// processing another message block, and updating the context.
//
void appMD5Update( FMD5Context* context, BYTE* input, INT inputLen )
{
	INT i, index, partLen;

	// Compute number of bytes mod 64.
	index = (INT)((context->count[0] >> 3) & 0x3F);

	// Update number of bits.
	if ((context->count[0] += ((DWORD)inputLen << 3)) < ((DWORD)inputLen << 3))
		context->count[1]++;
	context->count[1] += ((DWORD)inputLen >> 29);

	partLen = 64 - index;

	// Transform as many times as possible.
	if (inputLen >= partLen) {
		appMemcpy( &context->buffer[index], input, partLen );
		appMD5Transform( context->state, context->buffer );
		for (i = partLen; i + 63 < inputLen; i += 64)
			appMD5Transform( context->state, &input[i] );
		index = 0;
	} else
		i = 0;

	// Buffer remaining input.
	appMemcpy( &context->buffer[index], &input[i], inputLen-i );
}

//
// MD5 finalization. Ends an MD5 message-digest operation, writing the
// the message digest and zeroizing the context.
// Digest is 16 BYTEs.
//
void appMD5Final ( BYTE* digest, FMD5Context* context )
{
	BYTE bits[8];
	INT index, padLen;

	// Save number of bits.
	appMD5Encode( bits, context->count, 8 );

	// Pad out to 56 mod 64.
	index = (INT)((context->count[0] >> 3) & 0x3f);
	padLen = (index < 56) ? (56 - index) : (120 - index);
	appMD5Update( context, PADDING, padLen );

	// Append length (before padding).
	appMD5Update( context, bits, 8 );

	// Store state in digest
	appMD5Encode( digest, context->state, 16 );

	// Zeroize sensitive information.
	appMemset( context, 0, sizeof(*context) );
}

//
// MD5 basic transformation. Transforms state based on block.
//
void appMD5Transform( DWORD* state, BYTE* block )
{
	DWORD a = state[0], b = state[1], c = state[2], d = state[3], x[16];

	appMD5Decode( x, block, 64 );

	// Round 1
	FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
	FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
	FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
	FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
	FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
	FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
	FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
	FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
	FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
	FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
	FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
	FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
	FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
	FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
	FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
	FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

	// Round 2
	GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
	GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
	GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
	GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
	GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
	GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
	GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
	GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
	GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
	GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
	GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
	GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
	GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
	GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
	GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
	GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

	// Round 3
	HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
	HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
	HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
	HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
	HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
	HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
	HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
	HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
	HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
	HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
	HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
	HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
	HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
	HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
	HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
	HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

	// Round 4
	II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
	II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
	II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
	II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
	II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
	II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
	II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
	II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
	II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
	II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
	II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
	II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
	II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
	II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
	II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
	II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

	state[0] += a;
	state[1] += b;
	state[2] += c;
	state[3] += d;

	// Zeroize sensitive information.
	appMemset( x, 0, sizeof(x) );
}

//
// Encodes input (DWORD) into output (BYTE).
// Assumes len is a multiple of 4.
//
void appMD5Encode( BYTE* output, DWORD* input, INT len )
{
	INT i, j;

	for (i = 0, j = 0; j < len; i++, j += 4) {
		output[j] = (BYTE)(input[i] & 0xff);
		output[j+1] = (BYTE)((input[i] >> 8) & 0xff);
		output[j+2] = (BYTE)((input[i] >> 16) & 0xff);
		output[j+3] = (BYTE)((input[i] >> 24) & 0xff);
	}
}

//
// Decodes input (BYTE) into output (DWORD).
// Assumes len is a multiple of 4.
//
void appMD5Decode( DWORD* output, BYTE* input, INT len )
{
	INT i, j;

	for (i = 0, j = 0; j < len; i++, j += 4)
		output[i] = ((DWORD)input[j]) | (((DWORD)input[j+1]) << 8) |
		(((DWORD)input[j+2]) << 16) | (((DWORD)input[j+3]) << 24);
}

/*-----------------------------------------------------------------------------
	Exceptions.
-----------------------------------------------------------------------------*/

//
// Throw a string exception with a message.
//
VARARG_BODY( void VARARGS, appThrowf, const TCHAR*, VARARG_NONE )
{
	static TCHAR TempStr[4096];
	GET_VARARGS(TempStr,ARRAY_COUNT(TempStr),Fmt,Fmt);
	throw( TempStr );
}

/*-----------------------------------------------------------------------------
	Parameter parsing.
-----------------------------------------------------------------------------*/

//
// Get a string from a text string.
//
UBOOL Parse
(
	const TCHAR* Stream, 
	const TCHAR* Match,
	TCHAR*		 Value,
	INT			 MaxLen
)
{
	const TCHAR* Found = appStrfind(Stream,Match);
	const TCHAR* Start;

	if( Found )
	{
		Start = Found + appStrlen(Match);
		if( *Start == '\x22' )
		{
			// Quoted string with spaces.
			appStrncpy( Value, Start+1, MaxLen );
			Value[MaxLen-1]=0;
			TCHAR* Temp = appStrstr( Value, TEXT("\x22") );
			if( Temp != NULL )
				*Temp=0;
		}
		else
		{
			// Non-quoted string without spaces.
			appStrncpy( Value, Start, MaxLen );
			Value[MaxLen-1]=0;
			TCHAR* Temp;
			Temp = appStrstr( Value, TEXT(" ")  ); if( Temp ) *Temp=0;
			Temp = appStrstr( Value, TEXT("\r") ); if( Temp ) *Temp=0;
			Temp = appStrstr( Value, TEXT("\n") ); if( Temp ) *Temp=0;
			Temp = appStrstr( Value, TEXT("\t") ); if( Temp ) *Temp=0;
			Temp = appStrstr( Value, TEXT(",")  ); if( Temp ) *Temp=0;
		}
		return 1;
	}
	else return 0;
}

//
// See if a command-line parameter exists in the stream.
//
UBOOL ParseParam( const TCHAR* Stream, const TCHAR* Param )
{
	const TCHAR* Start = Stream;
	if( *Stream )
		while( (Start=appStrfind(Start+1,Param)) != NULL )
			if( Start>Stream && (Start[-1]=='-' || Start[-1]=='/') )
				return 1;
	return 0;
}

// 
// Parse a string.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, FString& Value )
{
	TCHAR Temp[4096]=TEXT("");
	if( ::Parse( Stream, Match, Temp, ARRAY_COUNT(Temp) ) )
	{
		Value = Temp;
		return 1;
	}
	else return 0;
}

//
// Parse a quadword.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, QWORD& Value )
{
	return Parse( Stream, Match, *(SQWORD*)&Value );
}

//
// Parse a signed quadword.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, SQWORD& Value )
{
	TCHAR Temp[4096]=TEXT(""), *Ptr=Temp;
	if( ::Parse( Stream, Match, Temp, ARRAY_COUNT(Temp) ) )
	{
		Value = 0;
		UBOOL Negative = (*Ptr=='-');
		Ptr += Negative;
		while( *Ptr>='0' && *Ptr<='9' )
			Value = Value*10 + *Ptr++ - '0';
		if( Negative )
			Value = -Value;
		return 1;
	}
	else return 0;
}

//
// Get a name.
//
UBOOL Parse
(
	const TCHAR* Stream, 
	const TCHAR* Match, 
	FName& Name
)
{
	TCHAR TempStr[NAME_SIZE];

	if( !Parse(Stream,Match,TempStr,NAME_SIZE) )
		return 0;
	Name = FName( TempStr );

	return 1;
}

//
// Get a DWORD.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, DWORD& Value )
{
	const TCHAR* Temp = appStrfind(Stream,Match);
	TCHAR* End;
	if( Temp==NULL )
		return 0;
	Value = appStrtoi( Temp + appStrlen(Match), &End, 10 );

	return 1;
}

//
// Get a byte.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, BYTE& Value )
{
	const TCHAR* Temp = appStrfind(Stream,Match);
	if( Temp==NULL )
		return 0;
	Temp += appStrlen( Match );
	Value = (BYTE)appAtoi( Temp );
	return Value!=0 || appIsDigit(Temp[0]);
}

//
// Get a signed byte.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, SBYTE& Value )
{
	const TCHAR* Temp = appStrfind(Stream,Match);
	if( Temp==NULL )
		return 0;
	Temp += appStrlen( Match );
	Value = appAtoi( Temp );
	return Value!=0 || appIsDigit(Temp[0]);
}

//
// Get a word.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, _WORD& Value )
{
	const TCHAR* Temp = appStrfind( Stream, Match );
	if( Temp==NULL )
		return 0;
	Temp += appStrlen( Match );
	Value = (_WORD)appAtoi( Temp );
	return Value!=0 || appIsDigit(Temp[0]);
}

//
// Get a signed word.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, SWORD& Value )
{
	const TCHAR* Temp = appStrfind( Stream, Match );
	if( Temp==NULL )
		return 0;
	Temp += appStrlen( Match );
	Value = (SWORD)appAtoi( Temp );
	return Value!=0 || appIsDigit(Temp[0]);
}

//
// Get a floating-point number.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, FLOAT& Value )
{
	const TCHAR* Temp = appStrfind( Stream, Match );
	if( Temp==NULL )
		return 0;
	Value = appAtof( Temp+appStrlen(Match) );
	return 1;
}

//
// Get a signed double word.
//
UBOOL Parse( const TCHAR* Stream, const TCHAR* Match, INT& Value )
{
	const TCHAR* Temp = appStrfind( Stream, Match );
	if( Temp==NULL )
		return 0;
	Value = appAtoi( Temp + appStrlen(Match) );
	return 1;
}

//
// Get a boolean value.
//
UBOOL ParseUBOOL( const TCHAR* Stream, const TCHAR* Match, UBOOL& OnOff )
{
	TCHAR TempStr[16];
	if( Parse( Stream, Match, TempStr, 16 ) )
	{
		OnOff
		=	!appStricmp(TempStr,TEXT("On"))
		||	!appStricmp(TempStr,TEXT("True"))
		||	!appStricmp(TempStr,GTrue)
		||	!appStricmp(TempStr,TEXT("1"));
		return 1;
	}
	else return 0;
}

//
// Sees if Stream starts with the named command.  If it does,
// skips through the command and blanks past it.  Returns 1 of match,
// 0 if not.
//
UBOOL ParseCommand
(
	const TCHAR** Stream, 
	const TCHAR*  Match
)
{
	while( (**Stream==' ')||(**Stream==9) )
		(*Stream)++;

	if( appStrnicmp(*Stream,Match,appStrlen(Match))==0 )
	{
		*Stream += appStrlen(Match);
		if( !appIsAlnum(**Stream) )
		{
			while ((**Stream==' ')||(**Stream==9)) (*Stream)++;
			return 1; // Success.
		}
		else
		{
			*Stream -= appStrlen(Match);
			return 0; // Only found partial match.
		}
	}
	else return 0; // No match.
}

//
// Get next command.  Skips past comments and cr's.
//
void ParseNext( const TCHAR** Stream )
{
	// Skip over spaces, tabs, cr's, and linefeeds.
	SkipJunk:
	while( **Stream==' ' || **Stream==9 || **Stream==13 || **Stream==10 )
		++*Stream;

	if( **Stream==';' )
	{
		// Skip past comments.
		while( **Stream!=0 && **Stream!=10 && **Stream!=13 )
			++*Stream;
		goto SkipJunk;
	}

	// Upon exit, *Stream either points to valid Stream or a nul.
}

//
// Grab the next space-delimited string from the input stream.
// If quoted, gets entire quoted string.
//
UBOOL ParseToken( const TCHAR*& Str, TCHAR* Result, INT MaxLen, UBOOL UseEscape )
{
	INT Len=0;

	// Skip spaces and tabs.
	while( *Str==' ' || *Str==9 )
		Str++;
	if( *Str == 34 )
	{
		// Get quoted string.
		Str++;
		while( *Str && *Str!=34 && (Len+1)<MaxLen )
		{
			TCHAR c = *Str++;
			if( c=='\\' && UseEscape )
			{
				// Get escape.
				c = *Str++;
				if( !c )
					break;
			}
			if( (Len+1)<MaxLen )
				Result[Len++] = c;
		}
		if( *Str==34 )
			Str++;
	}
	else
	{
		// Get unquoted string.
		for( ; *Str && *Str!=' ' && *Str!=9; Str++ )
			if( (Len+1)<MaxLen )
				Result[Len++] = *Str;
	}
	Result[Len]=0;
	return Len!=0;
}
UBOOL ParseToken( const TCHAR*& Str, FString& Arg, UBOOL UseEscape )
{
	TCHAR Buffer[1024];
	if( ParseToken( Str, Buffer, ARRAY_COUNT(Buffer), UseEscape ) )
	{
		Arg = Buffer;
		return 1;
	}
	return 0;
}
FString ParseToken( const TCHAR*& Str, UBOOL UseEscape )
{
	TCHAR Buffer[1024];
	if( ParseToken( Str, Buffer, ARRAY_COUNT(Buffer), UseEscape ) )
		return Buffer;
	else
		return TEXT("");
}

FLOAT ParseNextFloat(const TCHAR** Stream)
{
	const TCHAR* S = *Stream;
	while (*S == ' ')
		++S;
	FLOAT Result = appAtof(S);
	while (*S && *S != ' ')
		++S;
	*Stream = S;
	return Result;
}

//
// Get a line of Stream (everything up to, but not including, CR/LF.
// Returns 0 if ok, nonzero if at end of stream and returned 0-length string.
//
UBOOL ParseLine
(
	const TCHAR**	Stream,
	TCHAR*			Result,
	INT				MaxLen,
	UBOOL			Exact
)
{
	UBOOL GotStream=0;
	UBOOL IsQuoted=0;
	UBOOL Ignore=0;

	*Result=0;
	while( **Stream!=0 && **Stream!=10 && **Stream!=13 && --MaxLen>0 )
	{
		// Start of comments.
		if( !IsQuoted && !Exact && (*Stream)[0]=='/' && (*Stream)[1]=='/' )
			Ignore = 1;
		
		// Command chaining.
		if( !IsQuoted && !Exact && **Stream=='|' )
			break;

		// Check quoting.
		IsQuoted = IsQuoted ^ (**Stream==34);
		GotStream=1;

		// Got stuff.
		if( !Ignore )
			*(Result++) = *((*Stream)++);
		else
			(*Stream)++;
	}
	if( Exact )
	{
		// Eat up exactly one CR/LF.
		if( **Stream == 13 )
			(*Stream)++;
		if( **Stream == 10 )
			(*Stream)++;
	}
	else
	{
		// Eat up all CR/LF's.
		while( **Stream==10 || **Stream==13 || **Stream=='|' )
			(*Stream)++;
	}
	*Result=0;
	return **Stream!=0 || GotStream;
}
UBOOL ParseLine
(
	const TCHAR**	Stream,
	FString&		Result,
	UBOOL			Exact
)
{
	TCHAR Temp[4096]=TEXT("");
	UBOOL Success = ParseLine( Stream, Temp, ARRAY_COUNT(Temp), Exact );
	Result = Temp;
	return Success;
}

/*----------------------------------------------------------------------------
	String substitution.
----------------------------------------------------------------------------*/

FString appFormat( FString Src, const TMultiMap<FString,FString>& Map )
{
	FString Result;
	for( INT Toggle=0; ; Toggle^=1 )
	{
		INT Pos=Src.InStr(TEXT("%")), NewPos=Pos>=0 ? Pos : Src.Len();
		FString Str = Src.Left( NewPos );
		if( Toggle )
		{
			const FString* Ptr = Map.Find( Str );
			if( Ptr )
				Result += *Ptr;
			else if( NewPos!=Src.Len() )
				Result += US + TEXT("%") + Str + TEXT("%");
			else
				Result += US + TEXT("%") + Str;
		}
		else Result += Str;
		Src = Src.Mid( NewPos+1 );
		if( Pos<0 )
			break;
	}
	return Result;
}

/*-----------------------------------------------------------------------------
	High level file functions.
-----------------------------------------------------------------------------*/

//
// Update file modification time.
//
UBOOL appUpdateFileModTime( TCHAR* Filename )
{
	FArchive* Ar = GFileManager->CreateFileWriter(Filename,FILEWRITE_Append,GNull);
	if( Ar )
	{
		delete Ar;
		return 1;
	}
	return 0;
}

//
// Load a binary file to a dynamic array.
//
UBOOL appLoadFileToArray( TArray<BYTE>& Result, const TCHAR* Filename, FFileManager* FileManager )
{
	FArchive* Reader = FileManager->CreateFileReader( Filename );
	if( !Reader )
		return 0;
	Result.Empty();
	Result.Add( Reader->TotalSize() );
	Reader->Serialize( &Result(0), Result.Num() );
	UBOOL Success = Reader->Close();
	delete Reader;
	return Success;
}

//
// Load a text file to an FString.
// Supports all combination of ANSI/Unicode files and platforms.
//
UBOOL appLoadFileToString( FString& Result, const TCHAR* Filename, FFileManager* FileManager )
{
	FArchive* Reader = FileManager->CreateFileReader( Filename );
	if( !Reader )
		return 0;
	INT Size = Reader->TotalSize();
	TArray<ANSICHAR> Ch( Size+2 );
	Reader->Serialize( &Ch(0), Size );
	UBOOL Success = Reader->Close();
	delete Reader;
	Ch( Size+0 )=0;
	Ch( Size+1 )=0;
	TArray<TCHAR>& ResultArray = Result.GetCharArray();
	ResultArray.Empty();
	if( Size>=2 && !(Size&1) && (BYTE)Ch(0)==0xff && (BYTE)Ch(1)==0xfe )
	{
		// Unicode Intel byte order.
		ResultArray.Add( Size/sizeof(TCHAR) );
		for( INT i=0; i<ResultArray.Num()-1; i++ )
			ResultArray( i ) = FromUnicode( (_WORD)(ANSICHARU)Ch(i*2+2) + (_WORD)(ANSICHARU)Ch(i*2+3)*256 );
	}
	else if( Size>=2 && !(Size&1) && (BYTE)Ch(0)==0xfe && (BYTE)Ch(1)==0xff )
	{
		// Unicode non-Intel byte order.
		ResultArray.Add( Size/sizeof(TCHAR) );
		for( INT i=0; i<ResultArray.Num()-1; i++ )
			ResultArray( i ) = FromUnicode( (_WORD)(ANSICHARU)Ch(i*2+3) + (_WORD)(ANSICHARU)Ch(i*2+2)*256 );
	}
	else
	{
		// ANSI.
		ResultArray.Add( Size+1 );
		for( INT i=0; i<ResultArray.Num()-1; i++ )
			ResultArray( i ) = FromAnsi( Ch(i) );
	}
	ResultArray.Last() = 0;
	return Success;
}

//
// Save a binary array to a file.
//
UBOOL appSaveArrayToFile( const TArray<BYTE>& Array, const TCHAR* Filename, FFileManager* FileManager )
{
	FArchive* Ar = FileManager->CreateFileWriter( Filename );
	if( !Ar )
		return 0;
	Ar->Serialize( const_cast<BYTE*>(&Array(0)), Array.Num() );
	delete Ar;
	return 1;
}

//
// Write the FString to a file.
// Supports all combination of ANSI/Unicode files and platforms.
//
UBOOL appSaveStringToFile( const FString& String, const TCHAR* Filename, FFileManager* FileManager )
{
	if( !String.Len() )
		return 0;
	FArchive* Ar = FileManager->CreateFileWriter( Filename );
	if( !Ar )
		return 0;
	UBOOL SaveAsUnicode=0, Success=1;
#if UNICODE
	for( INT i=0; i<String.Len(); i++ )
	{
		if( (*String)[i] != (TCHAR)(ANSICHARU)ToAnsi((*String)[i]) )
		{
			UNICHAR BOM = UNICODE_BOM;
			Ar->Serialize( &BOM, sizeof(BOM) );
			SaveAsUnicode = 1;
			break;
		}
	}
#endif
	if( SaveAsUnicode || sizeof(TCHAR)==1 )
	{
		Ar->Serialize( const_cast<TCHAR*>(*String), String.Len()*sizeof(TCHAR) );
	}
	else
	{
		TArray<ANSICHAR> AnsiBuffer(String.Len());
		for( INT i=0; i<String.Len(); i++ )
			AnsiBuffer(i) = ToAnsi((*String)[i]);
		Ar->Serialize( const_cast<ANSICHAR*>(&AnsiBuffer(0)), String.Len() );
	}
	delete Ar;
	if( !Success )
		GFileManager->Delete( Filename );
	return Success;
}

static INT BitmapIndex = -1;
UBOOL appCreateBitmap( const TCHAR* Pattern, INT Width, INT Height, FColor* Data, FFileManager* FileManager )
{
	TCHAR File[64];

	if( BitmapIndex == -1 )
	{
		for( INT TestBitmapIndex=0; TestBitmapIndex<65536; TestBitmapIndex++ )
		{
			appSprintf( File, TEXT("%05i%s.bmp"), TestBitmapIndex, Pattern );
			if( FileManager->FileSize(File) < 0 )
			{
				BitmapIndex = TestBitmapIndex;
				break;
			}
		}
	}

	appSprintf( File, TEXT("%05i%s.bmp"), BitmapIndex++, Pattern );

	if( FileManager->FileSize(File)<0 )
	{
		FArchive* Ar = FileManager->CreateFileWriter( File );
		if( Ar )
		{
			// Types.
			#if SUPPORTS_PRAGMA_PACK
				#pragma pack (push,1)
			#endif
			struct BITMAPFILEHEADER
			{
				_WORD   bfType GCC_PACK(1);
				DWORD   bfSize GCC_PACK(1);
				_WORD   bfReserved1 GCC_PACK(1); 
				_WORD   bfReserved2 GCC_PACK(1);
				DWORD   bfOffBits GCC_PACK(1);
			} FH; 
			struct BITMAPINFOHEADER
			{
				DWORD  biSize GCC_PACK(1); 
				INT    biWidth GCC_PACK(1);
				INT    biHeight GCC_PACK(1);
				_WORD  biPlanes GCC_PACK(1);
				_WORD  biBitCount GCC_PACK(1);
				DWORD  biCompression GCC_PACK(1);
				DWORD  biSizeImage GCC_PACK(1);
				INT    biXPelsPerMeter GCC_PACK(1); 
				INT    biYPelsPerMeter GCC_PACK(1);
				DWORD  biClrUsed GCC_PACK(1);
				DWORD  biClrImportant GCC_PACK(1); 
			} IH;
			#if SUPPORTS_PRAGMA_PACK
				#pragma pack (pop)
			#endif

			UINT	BytesPerLine = Align(Width * 3,4);

			// File header.
			FH.bfType       		= INTEL_ORDER16((_WORD) ('B' + 256*'M'));
			FH.bfSize       		= INTEL_ORDER32((DWORD) (sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + BytesPerLine * Height));
			FH.bfReserved1  		= INTEL_ORDER16((_WORD) 0);
			FH.bfReserved2  		= INTEL_ORDER16((_WORD) 0);
			FH.bfOffBits    		= INTEL_ORDER32((DWORD) (sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER)));
			Ar->Serialize( &FH, sizeof(FH) );

			// Info header.
			IH.biSize               = INTEL_ORDER32((DWORD) sizeof(BITMAPINFOHEADER));
			IH.biWidth              = INTEL_ORDER32((DWORD) Width);
			IH.biHeight             = INTEL_ORDER32((DWORD) Height);
			IH.biPlanes             = INTEL_ORDER16((_WORD) 1);
			IH.biBitCount           = INTEL_ORDER16((_WORD) 24);
			IH.biCompression        = INTEL_ORDER32((DWORD) 0); //BI_RGB
			IH.biSizeImage          = INTEL_ORDER32((DWORD) BytesPerLine * Height);
			IH.biXPelsPerMeter      = INTEL_ORDER32((DWORD) 0);
			IH.biYPelsPerMeter      = INTEL_ORDER32((DWORD) 0);
			IH.biClrUsed            = INTEL_ORDER32((DWORD) 0);
			IH.biClrImportant       = INTEL_ORDER32((DWORD) 0);
			Ar->Serialize( &IH, sizeof(IH) );

			// Colors.
			for( INT i=Height-1; i>=0; i-- )
			{
				for( INT j=0; j<Width; j++ )
				{
					Ar->Serialize( &Data[i*Width+j].B, 1 );
					Ar->Serialize( &Data[i*Width+j].G, 1 );
					Ar->Serialize( &Data[i*Width+j].R, 1 );
				}

				// Pad each row's length to be a multiple of 4 bytes.

				for(UINT PadIndex = Width * 3;PadIndex < BytesPerLine;PadIndex++)
				{
					BYTE	B = 0;
					Ar->Serialize(&B,1);
				}
			}

			// Success.
			delete Ar;
		}
		else 
			return 0;
	}
	else 
		return 0;

	// Success.
	return 1;
}


/*-----------------------------------------------------------------------------
	Files.
-----------------------------------------------------------------------------*/

FString FPackageFileCache::PackageFromPath( const TCHAR* InPathName )
{
	FString PackageName = InPathName;
	INT i = PackageName.InStr( PATH_SEPARATOR, 1 );
	if( i != -1 )
		PackageName = PackageName.Mid(i+1);

	i = PackageName.InStr( TEXT("/"), 1 );
	if( i != -1 )
		PackageName = PackageName.Mid(i+1);

	i = PackageName.InStr( TEXT("\\"), 1 );
	if( i != -1 )
		PackageName = PackageName.Mid(i+1);

	i = PackageName.InStr( TEXT(".") );
	if( i != -1 )
		PackageName = PackageName.Left(i);
	PackageName = PackageName.Locs();

	return PackageName;
}

void FPackageFileCache::SplitPath( const TCHAR* InPathName, FString& Path, FString& Filename, FString& Extension )
{
	Filename = InPathName;
	for( TCHAR* p=(TCHAR*)*Filename; *p; p++ )
		if( *p == '\\' || *p == '/' )
			*p = PATH_SEPARATOR[0];

	INT i = Filename.InStr( PATH_SEPARATOR, 1 );
	if( i != -1 )
	{
		Path = Filename.Left(i);
		Filename = Filename.Mid(i+1);
	}
	else
		Path = TEXT("");

	i = Filename.InStr( TEXT("."), 1 );
	if( i != -1 )
	{
		Extension = Filename.Mid(i+1);
		Filename = Filename.Left(i);
	}
	else
		Extension = TEXT("");
}

//
// Create a temporary file.
//
void appCreateTempFilename( const TCHAR* Path, TCHAR* Result256 )
{
	static INT i=0;
	do
		appSprintf( Result256, TEXT("%s%04X.tmp"), Path, i++ );
	while( GFileManager->FileSize(Result256)>0 );
}

/*-----------------------------------------------------------------------------
	Game/ mode specific directories.
-----------------------------------------------------------------------------*/

/* ============================================================================
 * appGameDir
 * 
 * Returns the base directory of the current game by looking at the global
 * GGameName variable. This is usually a subdirectory of the installation
 * root directory and can be overridden on the command line to allow self
 * contained mod support.
 *
 * ============================================================================
 */
FString appGameDir()
{
	return FString::Printf( TEXT("..") PATH_SEPARATOR TEXT("%sGame") PATH_SEPARATOR, GGameName );
}

/* ============================================================================
 * appGameConfigDir
 * 
 * Returns the directory the engine uses to look for the leaf ini files. This
 * can't be an .ini variable for obvious reasons.
 *
 * ============================================================================
 */
FString appGameConfigDir()
{
	return appGameDir() + TEXT("Config") PATH_SEPARATOR;
}

/* ============================================================================
 * appGameLogDir
 * 
 * Returns the directory the engine uses to output logs. This currently can't 
 * be an .ini setting as the game starts logging before it can read from .ini
 * files.
 *
 * ============================================================================
 */
FString appGameLogDir()
{
	return appGameDir() + TEXT("Logs") PATH_SEPARATOR;
}

/* ============================================================================
 * appEngineDir
 * 
 * Returns the base directory of the "core" engine that can be shared across
 * several games or across games & mods. Shaders and base localization files
 * e.g. reside in the engine directory.
 *
 * ============================================================================
 */
FString appEngineDir()
{
	return TEXT("..") PATH_SEPARATOR TEXT("EngineBase") PATH_SEPARATOR;
}

/* ============================================================================
 * appEngineConfigDir
 * 
 * Returns the directory the root configuration files are located.
 *
 * ============================================================================
 */
FString appEngineConfigDir()
{
	return appEngineDir() + TEXT("Config") + PATH_SEPARATOR;
}

/* ============================================================================
 * appAssembleIni
 * 
 * Given a Destination and Source filename, this function will recurse the
 * Destination till it hits the root and then merge ini settings from there
 * till the leaf (Source) and write out the result to the file specified by 
 * Destination.
 *
 * ============================================================================
 */
void appAssembleIni( const TCHAR* Destination, const TCHAR* Source )
{
	// Create from base inis if it doesn't exist.
	if( GFileManager->FileSize(Destination) < 0 )
	{
		FConfigFile		EngineConfig;
	
		// Keep a list of ini's, starting with Source and ending with the root configuration file.
		TArray<FString> IniList;
						IniList.AddItem( FString(Source) );
		INT IniIndex =	IniList.AddZeroed();

		// Recurse inis till we found a root ini (aka one without BasedOn being set).
		while( GConfig->GetString( TEXT("Configuration"), TEXT("BasedOn"), IniList(IniIndex), *IniList(IniIndex-1) ) )
		{
			IniIndex = IniList.AddZeroed();
		}

		// Discard empty entry.
		IniIndex--;

		// Spit out friendly error if there was a problem locating .inis (e.g. bad command line parameter or missing folder, ...).
		if( GFileManager->FileSize( *IniList(IniIndex) ) < 0 )
		{
			GConfig = NULL;
			appErrorf( NAME_FriendlyError, TEXT("Couldn't locate '%s' which is required to run '%s'"), *IniList(IniIndex), GGameName );
		}

		// Read root ini.
		EngineConfig.Read( *IniList(IniIndex) );

		// Traverse ini list back to front, merging along the way.
		for( IniIndex--; IniIndex >= 0; IniIndex-- )
		{
			EngineConfig.Combine( *IniList(IniIndex) );
		}

		// Finally write out the merged ini to disk. Requires explicitely marking as dirty so the case of just copying an ini works correctly.
		EngineConfig.Dirty = 1;
		EngineConfig.Write( Destination );
	}
}

/*-----------------------------------------------------------------------------
	Init and Exit.
-----------------------------------------------------------------------------*/

//
// General initialization.
//
TCHAR GCmdLine[4096]=TEXT("");
const TCHAR* appCmdLine()
{
	return GCmdLine;
}

void appInit( const TCHAR* InCmdLine, FMalloc* InMalloc, FOutputDevice* InLog, FOutputDeviceError* InError, FFeedbackContext* InWarn, FFileManager* InFileManager, FConfigCache*(*ConfigFactory)() )
{
	GFileManager = InFileManager;

	// Init CRC table.
    for( DWORD iCRC=0; iCRC<256; iCRC++ )
		for( DWORD c=iCRC<<24, j=8; j!=0; j-- )
			GCRCTable[iCRC] = c = c & 0x80000000 ? (c << 1) ^ CRC32_POLY : (c << 1);

	// Init log table.
	{for( INT i=0,e=-1,c=0; i<=256; i++ )
	{
		GLogs[i] = e+1;
		if( !i || ++c>=(1<<e) )
			c=0, e++;
	}}

	// Command line.
	appStrncpy( GCmdLine, InCmdLine, ARRAY_COUNT(GCmdLine) );

	// Error history.
	appStrcpy( GErrorHist, TEXT("General protection fault!\r\n\r\nHistory: ") );

	// Memory allocator.
	GMalloc = InMalloc;
	GMalloc->Init( 0 );

	// Output devices.
	GError		= InError;
	GLog->AddOutputDevice( InLog );

	// Feedback context.
	GWarn        = InWarn;
	if( ParseParam(appCmdLine(),TEXT("WARNINGSASERRORS")) )
		GWarn->TreatWarningsAsErrors = 1;

	// Switch into executable's directory.
	GFileManager->Init(1);
	GFileManager->SetDefaultDirectory();

	// Init names.
	FName::StaticInit();

	// Show log if wanted.
	if( GLogConsole && ParseParam(appCmdLine(),TEXT("LOG")) )
		GLogConsole->Show( TRUE );

	// Platform specific pre-init.
	appPlatformPreInit();

	// Command line.
	debugf( NAME_Init, TEXT("Command line: %s"), appCmdLine() );
	debugf( NAME_Init, TEXT("Base directory: %s"), appBaseDir() );
	debugf( NAME_Init, TEXT("Character set: %s"), sizeof(TCHAR)==1 ? TEXT("ANSI") : TEXT("Unicode") );
	
	// Init config.
	GConfig = ConfigFactory();

	// Memory initalization.
	GMem.Init( 65536 );

	// Platform specific init.
	appPlatformInit();
}

//
// Pre-shutdown.
// Called from within guarded exit code, only during non-error exits.
//
void appPreExit()
{
	debugf( NAME_Exit, TEXT("Preparing to exit.") );
	GMem.Exit();
}

//
// Shutdown.
// Called outside guarded exit code, during all exits (including error exits).
//
void appExit()
{
	debugf( NAME_Exit, TEXT("Exiting.") );
	if( GConfig )
	{
		GConfig->Exit();
		delete GConfig;
		GConfig = NULL;
	}
	FName::StaticExit();
	GLog->TearDown();
	GLog = NULL;
}

/*-----------------------------------------------------------------------------
	The End.
-----------------------------------------------------------------------------*/

