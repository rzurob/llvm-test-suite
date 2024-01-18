!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : varIsArrSectExprIsScalar05 - expr is a
!*                               Scalar and variable is an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  2, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section (with a Vector Subscript)
!*                               from an Allocated ALLOCATABLE Array of
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Type
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM varIsArrSectExprIsScalar05

    CHARACTER(6) :: char6Var = 'LmNoPq'
    CHARACTER(5) :: charArr( 10 )

    CHARACTER(:), ALLOCATABLE :: defArrAlloc( : )
    CHARACTER(5), ALLOCATABLE :: charArrAlloc( : )


    DO i = 1, 10
        WRITE(charArr( i ), '(I5)') (15555 + i)
    END DO


    defArrAlloc = charArr
    charArrAlloc = charArr
    CALL Dump( 10_4 )


    charArr( [ 1,3,5,7,9 ] ) = 'VwXyZ'
    defArrAlloc( [ 1,3,5,7,9 ] ) = 'VwXyZ'
    charArrAlloc( [ 1,3,5,7,9 ] ) = 'VwXyZ'
    CALL Dump( 20_4 )

    charArr( [ (i, i = 2, 10, 2) ] ) = 'AbCd'
    defArrAlloc( [ (i, i = 2, 10, 2) ] ) = 'AbCd'
    charArrAlloc( [ (i, i = 2, 10, 2) ] ) = 'AbCd'
    CALL Dump( 30_4 )

    charArr( [ 2,9 ] ) = char6Var
    defArrAlloc( [ 2,9 ] ) = char6Var
    charArrAlloc( [ 2,9 ] ) = char6Var
    CALL Dump( 40_4 )

    charArr( [ 4,5,6,7 ] ) = charArr( 1 )
    defArrAlloc( [ 4,5,6,7 ] ) = defArrAlloc( 1 )
    charArrAlloc( [ 4,5,6,7 ] ) = charArrAlloc( 1 )
    CALL Dump( 50_4 )

    charArr( [ 5,8,2 ] ) = charArr( 8 )
    defArrAlloc( [ 5,8,2 ] ) = defArrAlloc( 8 )
    charArrAlloc( [ 5,8,2 ] ) = charArrAlloc( 8 )
    CALL Dump( 60_4 )


    CONTAINS


        SUBROUTINE Dump( failRC )
            INTEGER(4) :: failRC

            IF (.NOT. ALLOCATED( defArrAlloc ))  CALL zzrc( failRC )
            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( (failRC + 1_4) )

            PRINT *
            PRINT *, failRC, SIZE( defArrAlloc ), SIZE( charArrAlloc )

            PRINT 10, charArr( : )
            PRINT 10, defArrAlloc( : )
            PRINT 10, charArrAlloc( : )

10          FORMAT(",",10(A6,","))

            IF (SIZE( defArrAlloc ) /= 10)  CALL zzrc( (failRC + 2_4) )
            IF (SIZE( charArrAlloc ) /= 10) CALL zzrc( (failRC + 3_4) )

            IF (LEN( defArrAlloc ) /= 5)    CALL zzrc( (failRC + 4_4) )
            IF (LEN( charArrAlloc ) /= 5)   CALL zzrc( (failRC + 5_4) )

            IF ( ANY( (defArrAlloc /= charArr) ) )  CALL zzrc( (failRC + 6_4) )
            IF ( ANY( (charArrAlloc /= charArr) ) ) CALL zzrc( (failRC + 7_4) )

        END SUBROUTINE Dump

END PROGRAM varIsArrSectExprIsScalar05
