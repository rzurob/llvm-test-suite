!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 3
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

MODULE mB
    TYPE tB
        INTEGER :: bI
    END TYPE tB
END MODULE mB

MODULE mD
    USE mB

    TYPE, EXTENDS(tB) :: tD
        INTEGER :: dI
    END TYPE tD
END MODULE mD

MODULE mD2
    USE mD

    TYPE, EXTENDS(tD) :: tD2
        INTEGER, POINTER :: d2IP
    END TYPE tD2
END MODULE mD2

PROGRAM unallocatedLBoundDerived03
    USE mD2

    INTERFACE
        SUBROUTINE CheckIt( dA )
            USE mD2
            TYPE(tD2), ALLOCATABLE :: dA( :,:,: )
        END SUBROUTINE CheckIt
    END INTERFACE

    INTEGER, TARGET :: iT( 100 ) = (/ (i, i = 0, 99) /)

    TYPE(tD2), ALLOCATABLE :: d2Alloc( :,:,: )

    TYPE(tD2) :: d2Array( 0:99,0:99,0:99 )

    DO i = 0, 99
        DO j = 0, 99
            DO k = 0, 99
                d2Array( k,j,i )%bI = k
                d2Array( k,j,i )%dI = j
                d2Array( k,j,i )%d2IP => iT( (i + 1) )
            END DO
        END DO
    END DO

    IF ( ALLOCATED( d2Alloc ) ) ERROR STOP 10_4

    d2Alloc = d2Array

    IF (.NOT. ALLOCATED( d2Alloc )) ERROR STOP 20_4

    IF (SIZE( d2Alloc ) /= 1000000) ERROR STOP 21_4

    IF (SIZE(d2Alloc, 1) /= 100)    ERROR STOP 22_4
    IF (SIZE(d2Alloc, 2) /= 100)    ERROR STOP 23_4
    IF (SIZE(d2Alloc, 3) /= 100)    ERROR STOP 24_4

    IF (LBOUND(d2Alloc, 1) /= 0)    ERROR STOP 25_4
    IF (LBOUND(d2Alloc, 2) /= 0)    ERROR STOP 26_4
    IF (LBOUND(d2Alloc, 3) /= 0)    ERROR STOP 27_4

    CALL CheckIt( d2Alloc )

END PROGRAM unallocatedLBoundDerived03

SUBROUTINE CheckIt( dA )
    USE mD2

    TYPE(tD2), ALLOCATABLE :: dA( :,:,: )

    DO i = 0, 99
        DO j = 0, 99
            DO k = 0, 99
                IF (dA( k,j,i )%bI /= k) THEN
                    PRINT *, k, j, i, dA( k,j,i )%bI, k
                    ERROR STOP 100_4

                ELSE IF (dA( k,j,i )%dI /= j) THEN
                    PRINT *, k, j, i, dA( k,j,i )%dI, j
                    ERROR STOP 101_4

                ELSE IF (dA( k,j,i )%d2IP /= i) THEN
                    PRINT *, k, j, i, dA( k,j,i )%d2IP, i
                    ERROR STOP 102_4
                END IF
            END DO
        END DO
    END DO

END SUBROUTINE CheckIt
