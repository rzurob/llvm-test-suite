!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for a single Dimension
!*                               Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : TYPE, ALLOCATABLE Attribute, Intrinsic
!*                               Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
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

MODULE mBase

    TYPE tBase
        INTEGER :: b

        CONTAINS

            FINAL :: tBaseFinal

    END TYPE tBase

    CONTAINS

        SUBROUTINE tBaseFinal( o )
            TYPE(tBase), INTENT(inout) :: o( : )

            ERROR STOP 99_4

        END SUBROUTINE tBaseFinal

END MODULE mBase

PROGRAM unallocatedLBoundDerived01
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived
        INTEGER :: d
    END TYPE tDerived

    TYPE(tDerived) :: derivedArray( 0:1 ) =&
            (/ (tDerived(i,(i * i)), i = 1, 2) /)

    TYPE(tDerived), ALLOCATABLE :: derivedArrayAlloc( : )


    IF ( ALLOCATED( derivedArrayAlloc ) ) ERROR STOP 10_4

    CALL Assign( )


    IF (.NOT. ALLOCATED( derivedArrayAlloc )) ERROR STOP 20_4

    IF (SIZE( derivedArrayAlloc ) /= 2)     ERROR STOP 30_4
    IF (LBOUND(derivedArrayAlloc, 1) /= 0)  ERROR STOP 40_4

    IF (derivedArrayAlloc( 0 )%b /= 1)  ERROR STOP 50_4
    IF (derivedArrayAlloc( 0 )%d /= 1)  ERROR STOP 60_4

    IF (derivedArrayAlloc( 1 )%b /= 2)  ERROR STOP 70_4
    IF (derivedArrayAlloc( 1 )%d /= 4)  ERROR STOP 80_4

    CONTAINS

        SUBROUTINE Assign( )

            derivedArrayAlloc = derivedArray

        END SUBROUTINE Assign

END PROGRAM unallocatedLBoundDerived01
