!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : July 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE and
!*  SECONDARY FUNCTIONS TESTED : LBOUND(expr) returns a value other than "1" as
!*                               the Lower Bound for a single Dimension Array
!*
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

MODULE module
    INTEGER(8), ALLOCATABLE :: intArrayAlloc( : )

    CONTAINS

        SUBROUTINE allocArray( )

            ALLOCATE(intArrayAlloc( 7:12 ),&
                    SOURCE=(/ (INT(i, 8), i = 55, 60) /))

            IF (.NOT. ALLOCATED( intArrayAlloc )) CALL zzrc( 10_4 )
            IF (SIZE( intArrayAlloc ) /= 6)       CALL zzrc( 20_4 )
            IF (LBOUND(intArrayAlloc, 1) /= 7)    CALL zzrc( 30_4 )

        END SUBROUTINE allocArray

END MODULE module

PROGRAM allocatedLBound01
    USE module

    INTEGER(4) :: i

    INTEGER, DIMENSION( -5:5 ) :: intArray = (/ (i, i = 1, 11) /)


    CALL allocArray( )
    CALL intrinAssign( )


    IF (.NOT. ALLOCATED( intArrayAlloc )) CALL zzrc( 40_4 )

    IF (SIZE( intArrayAlloc ) /= 11) THEN
        PRINT *, "SIZE( intArrayAlloc ) =", SIZE( intArrayAlloc )
        CALL zzrc( 50_4 )
    END IF

    IF (LBOUND(intArrayAlloc, 1) /= -5) THEN
        PRINT *, "LBOUND(intArrayAlloc, 1) =", LBOUND(intArrayAlloc, 1)
        CALL zzrc( 60_4 )
    END IF

    DO i = -5, 5
        IF (intArrayAlloc( i ) /= intArray( i )) THEN
            PRINT *, "     intArray(", i, ") =", intArray( i )
            PRINT *, "intArrayAlloc(", i, ") =", intArrayAlloc( i )

            CALL zzrc( 70_4 + i )
        END IF
    END DO


    CONTAINS


        SUBROUTINE intrinAssign( )

            intArrayAlloc = intArray

        END SUBROUTINE intrinAssign

END PROGRAM allocatedLBound01
