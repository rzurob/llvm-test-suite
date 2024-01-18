!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d327545 - expr is a Scalar and variable is
!*                               an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ALLOC: Element from an Array Section
!*                               Assigned to that Section
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below allocates a 2 Element Array of a Derived Type
!*  (with an ALLOCATABLE Component), then performs 2 Intrinsic Assignments
!*  where variable is an Array Section, and:
!*
!*  1. expr doesn't contain an Element from within the Array Section.
!*  2. expr contains an Element from within the Array Section.
!*
!*  The result from the first assignment is as expected, however the second
!*  results in unallocated Elements within the Array Section.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM varIsArrSectExprIsScalar03

    TYPE :: tBase
        INTEGER, ALLOCATABLE :: bC
    END TYPE tBase

    TYPE(tBase), ALLOCATABLE :: tAlloc( : )

    tAlloc = [ (tBase(i), i = 1, 2) ]
    PRINT *, (tAlloc( i )%bC, i = 1, 2),(ALLOCATED( tAlloc( i )%bC ), i = 1, 2)

    tAlloc( 2: ) = tAlloc( 1 )
    PRINT *, (tAlloc( i )%bC, i = 1, 2),(ALLOCATED( tAlloc( i )%bC ), i = 1, 2)

    tAlloc( :1 ) = tAlloc( 1 )
    PRINT *, (tAlloc( i )%bC, i = 1, 2),(ALLOCATED( tAlloc( i )%bC ), i = 1, 2)

END PROGRAM varIsArrSectExprIsScalar03
