!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section from an Allocated ALLOCATABLE
!*                               Array,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Type as
!*                               variable
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 5
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM varIsArrSectExprIsScalar01

    REAL :: realScalar = -4.2
    REAL(8), ALLOCATABLE :: realArrAlloc( :,: )


    realArrAlloc = RESHAPE([ (((REAL(j, 8) + (REAL(i, 8) / 10._8)),&
                                    j = 0, 9), i = 0, 9) ], [ 10,10 ])
    CALL Dump( 10_4 )


    realArrAlloc( 4:7,7:4:-1 ) = -2.04_8
    CALL Dump( 20_4 )

    realArrAlloc( :,5 ) = realArrAlloc( 3,5 )
    CALL Dump( 30_4 )

    realArrAlloc( 5,: ) = realArrAlloc( 3,5 ) + realArrAlloc( 4,4 )
    CALL Dump( 40_4 )

    realArrAlloc( 1:3,2 ) = realScalar
    CALL Dump( 50_4 )

    realArrAlloc( 9,8:10 ) = REAL(realScalar, 16)
    CALL Dump( 60_4 )


    CONTAINS


        SUBROUTINE Dump( failRC )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( realArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, KIND( realArrAlloc ),&
                     SIZE(realArrAlloc, 1), SIZE(realArrAlloc, 2)

            DO i = 1, 10
                PRINT '(10F6.2)', realArrAlloc( i,: )
            END DO


            IF (KIND( realArrAlloc ) /= 8)   CALL zzrc( (failRC + 1_4) )

            IF (SIZE(realArrAlloc, 1) /= 10) CALL zzrc( (failRC + 2_4) )
            IF (SIZE(realArrAlloc, 2) /= 10) CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE Dump

END PROGRAM varIsArrSectExprIsScalar01
