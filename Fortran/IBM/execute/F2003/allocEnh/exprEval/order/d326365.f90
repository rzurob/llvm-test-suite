!*  ===================================================================
!*
!*  DATE                       : October  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: IMPDO: No Output for Allocated
!*                               ALLOCATABLE CHARACTER Array
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Implied-DO, PRINT, ALLOCATABLE, CHARACTER,
!*                               Deferred Length Parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below assigns a single Element Array to an Unallocated
!*  ALLOCATABLE CHARACTER with a Deferred Length Type Parameter, and
!*  dumps the value to STDOUT using a PRINT with an Implied-DO.  Output
!*  from this PRINT is terminated before the value is written to STDOUT.
!*  Other forms of the PRINT statement will correctly write this array
!*  to STDOUT.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM deferredArrExprVectIdx01

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( : )

    charArrAlloc = (/ '1' /)

    PRINT 10, charArrAlloc( 1 )
    PRINT 10, charArrAlloc
    PRINT 10, charArrAlloc( : )

    PRINT 10, (charArrAlloc( j ), j = 1, 1)
10  FORMAT("(",A1,")")

END PROGRAM deferredArrExprVectIdx01
