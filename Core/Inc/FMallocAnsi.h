/*=============================================================================
	FMallocAnsi.h: ANSI memory allocator.
	Copyright 1997-1999 Epic Games, Inc. All Rights Reserved.

	Revision history:
		* Created by Tim Sweeney
		* Alignment support by Daniel Vogel
=============================================================================*/

//
// ANSI C memory allocator.
//
class FMallocAnsi : public FMalloc
{
public:
	// FMalloc interface.
	void* Malloc( DWORD Size )
	{
		check(Size>=0);
		void* Ptr = malloc(Size);
		check(Ptr);
		return Ptr;
	}
	void* Realloc( void* Ptr, DWORD NewSize )
	{
		checkSlow(NewSize>=0);
		void* Result;
		if( Ptr && NewSize )
		{
			Result = realloc(Ptr, NewSize);
		}
		else if( Ptr == NULL )
		{
			Result = malloc(NewSize);
		}
		else
		{
			free(Ptr);
			Result = NULL;
		}
		return Result;
	}
	void Free( void* Ptr )
	{
		free( Ptr );
	}
	void DumpAllocs()
	{
		debugf( NAME_Exit, TEXT("Allocation checking disabled") );
	}
	void HeapCheck()
	{
#if (defined _MSC_VER)
		INT Result = _heapchk();
		check(Result!=_HEAPBADBEGIN);
		check(Result!=_HEAPBADNODE);
		check(Result!=_HEAPBADPTR);
		check(Result!=_HEAPEMPTY);
		check(Result==_HEAPOK);
#endif
	}
	void Init( UBOOL Reset )
	{
	}
	void Exit()
	{
	}
};

/*-----------------------------------------------------------------------------
	The End.
-----------------------------------------------------------------------------*/

