!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedAllocatable02 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : June  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : with a different Shape from expr (but does
!*                               have the same Length Type Parameter Values
!*                               as expr)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unAllocatedAllocatable02

    INTEGER(KIND = 4) :: i

    INTEGER, PARAMETER :: N = 10

    INTEGER, DIMENSION( : ), ALLOCATABLE :: integerArrayUnAlloc
    INTEGER, DIMENSION( N ) :: integerArray = (/ (i, i = 1, N) /)


    IF ( ALLOCATED( integerArrayUnAlloc ) ) THEN
        PRINT *, "ALLOCATED( integerArrayUnAlloc ) == .TRUE."
        CALL zzrc( 10_4 )
    END IF


    PRINT 100, integerArray

    integerArrayUnAlloc = integerArray
    IF (.NOT. ALLOCATED( integerArrayUnAlloc )) THEN
        PRINT *, "ALLOCATED( integerArrayUnAlloc ) == .FALSE."
        CALL zzrc( 20_4 )
    END IF

    PRINT 100, integerArrayUnAlloc


    DO i = 1, N
        IF (integerArrayUnAlloc( i ) /= integerArray( i )) THEN
            PRINT *, "integerArrayUnAlloc(", i, ") =",&
                                integerArrayUnAlloc( i )
            PRINT *, "integerArray(", i, ") =", integerArray( i )

            CALL zzrc( (30_4 + i) )
        END IF
    END DO


100 FORMAT('(',9(I2,','),I2,')')


END PROGRAM unAllocatedAllocatable02
