!*  ===================================================================
!*
!*                               Non-CHARACTER Array of Derived Type
!*
!*  DATE                       : August 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Array of Derived Type with the
!*                               same Shape and Length Type Parameter Values
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
    END TYPE tBase
END MODULE mBase

MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived
        INTEGER :: d
    END TYPE tDerived
END MODULE mDerived

MODULE mDerived2
    USE mDerived

    TYPE, EXTENDS(tDerived) :: tDerived2
        INTEGER :: d2
    END TYPE tDerived2
END MODULE mDerived2

MODULE mDerived3
    USE mDerived2

    TYPE, EXTENDS(tDerived2) :: tDerived3
        INTEGER :: d3
    END TYPE tDerived3

    TYPE(tDerived3) :: derived3Array( 3,3,3 ) =&
        RESHAPE((/ (tDerived3((i - 1),i,(i + 1),(i * 2)), i = 1, 27) /),&
                                                                (/ 3,3,3 /))
END MODULE mDerived3

MODULE mSubroutine
    USE mDerived3

    TYPE(tDerived3), ALLOCATABLE :: derived3ArrayAlloc( :,:,: )

    CONTAINS

        SUBROUTINE Assign( )

            derived3ArrayAlloc = derived3Array

        END SUBROUTINE Assign

END MODULE mSubroutine

PROGRAM unallocatedArrayDerived01
    USE mSubroutine

    INTEGER(4) :: val

    val = 0

    IF ( ALLOCATED( derived3ArrayAlloc ) ) CALL zzrc( 10_4 )

    CALL Assign( )

    IF (.NOT. ALLOCATED( derived3ArrayAlloc )) CALL zzrc( 20_4 )

    IF (SIZE( derived3ArrayAlloc ) /= 27) CALL zzrc( 30_4 )

    IF (SIZE(derived3ArrayAlloc, 1) /= 3) CALL zzrc( 31_4 )
    IF (SIZE(derived3ArrayAlloc, 2) /= 3) CALL zzrc( 32_4 )
    IF (SIZE(derived3ArrayAlloc, 3) /= 3) CALL zzrc( 33_4 )

    DO i = 1, 3
        DO j = 1, 3
            DO k = 1, 3
                val = val + 1

                PRINT *, k, j, i,&
                        derived3ArrayAlloc( k,j,i )%b,&
                        derived3ArrayAlloc( k,j,i )%d,&
                        derived3ArrayAlloc( k,j,i )%d2,&
                        derived3ArrayAlloc( k,j,i )%d3, val

                IF (derived3ArrayAlloc( k,j,i )%b /= (val - 1))&
                                            CALL zzrc( (100_4 + val) )
                IF (derived3ArrayAlloc( k,j,i )%d /= val)&
                                            CALL zzrc( (130_4 + val) )
                IF (derived3ArrayAlloc( k,j,i )%d2 /= (val + 1))&
                                            CALL zzrc( (160_4 + val) )
                IF (derived3ArrayAlloc( k,j,i )%d3 /= (val * 2))&
                                            CALL zzrc( (190_4 + val) )
            END DO
        END DO
    END DO

END PROGRAM unallocatedArrayDerived01
