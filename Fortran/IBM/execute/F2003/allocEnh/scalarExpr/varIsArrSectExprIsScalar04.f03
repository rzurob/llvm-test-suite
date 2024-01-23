!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : November  1, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section (with a Vector Subscript)
!*                               from an Allocated ALLOCATABLE Array
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Type
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
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

PROGRAM varIsArrSectExprIsScalar04

    INTEGER, ALLOCATABLE :: intArrAlloc( :,: )


    intArrAlloc =&
        RESHAPE([ ((((i * 10) + j), j = 0, 9), i = 0, 9) ], [ 10,10 ])
    CALL Dump( 10_4 )


    intArrAlloc( [ (i, i = 1, 10, 2) ],[ (i, i = 10, 1, -2) ] ) = -9
    CALL Dump( 20_4 )

    intArrAlloc( [ 2,4,6,8,10 ],: ) = -9
    CALL Dump( 30_4 )

    intArrAlloc( :,[ 1,3,5,7,9 ] ) = -9
    CALL Dump( 40_4 )


    IF ( ANY( (intArrAlloc /= -9) ) ) ERROR STOP 50_4


    intArrAlloc( [ 1,10 ],[ 1,10 ] ) = 7
    CALL Dump( 60_4 )


    IF ( ANY( (intArrAlloc( 2:9,2:9 ) /= -9) ) ) ERROR STOP 70_4

    IF ( ANY( (intArrAlloc( 1,2:9 ) /= -9) ) )   ERROR STOP 71_4
    IF ( ANY( (intArrAlloc( 2:9,1 ) /= -9) ) )   ERROR STOP 72_4

    IF ( ANY( (intArrAlloc( [ 1,10 ],[ 1,10 ] ) /= 7) ) ) ERROR STOP 73_4


    CONTAINS


        SUBROUTINE Dump( failRC )

            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, SIZE(intArrAlloc, 1), SIZE(intArrAlloc, 2)

            PRINT '(10(I3))',&
                  ((intArrAlloc( i,j ), j = 1, SIZE(intArrAlloc, 2)),&
                                            i = 1, SIZE(intArrAlloc, 1))


            IF (SIZE(intArrAlloc, 1) /= 10) CALL zzrc( (failRC + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= 10) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE Dump

END PROGRAM varIsArrSectExprIsScalar04
