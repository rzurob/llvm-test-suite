! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/allocEnh/scalarExpr/varIsArrSectExprIsScalar06.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : November  2, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section (with a Vector Subscript)
!*                               from an Allocated ALLOCATABLE Array of
!*                               Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Derived
!*                               Type
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

PROGRAM varIsArrSectExprIsScalar06

    TYPE :: tB(K1)    ! (4)
        INTEGER, KIND            :: K1
        INTEGER(K1), ALLOCATABLE :: b
    END TYPE tB

    TYPE :: tD1(K2)    ! (4)
        INTEGER, KIND :: K2
        TYPE(tB(K2))  :: b
    END TYPE tD1

    TYPE :: tD2(K3)    ! (4)
        INTEGER, KIND :: K3
        TYPE(tD1(K3)) :: d1
    END TYPE tD2

    TYPE(tD2(4)) :: d2Scalar
    TYPE(tD2(4)), ALLOCATABLE :: d2Arr( : )


    d2Scalar = tD2(4)(tD1(4)(tB(4)(99)))

!    d2Arr = [ (tD2(tD1(tB(i))), i = 1, 10) ]
    d2Arr = [ tD2(4)(tD1(4)(tB(4)(1))),tD2(4)(tD1(4)(tB(4)(2))),tD2(4)(tD1(4)(tB(4)(3))),&
              tD2(4)(tD1(4)(tB(4)(4))),tD2(4)(tD1(4)(tB(4)(5))),tD2(4)(tD1(4)(tB(4)(6))),&
              tD2(4)(tD1(4)(tB(4)(7))),tD2(4)(tD1(4)(tB(4)(8))),tD2(4)(tD1(4)(tB(4)(9))),tD2(4)(tD1(4)(tB(4)(10)))]
    CALL Dump( 10_4 )


    d2Arr( [ 2,4,6,8,10 ] ) = tD2(4)(tD1(4)(tB(4)(-9)))
    CALL Dump( 40_4 )

    d2Arr( [ 1,3,5,7,9 ] ) = d2Scalar
    CALL Dump( 70_4 )

    d2Arr( [ 2,8 ] ) = d2Arr( 5 )
    CALL Dump( 100_4 )

    d2Arr( [ 3,6,9 ] ) = d2Arr( 6 )
    CALL Dump( 130_4 )


    CONTAINS


        SUBROUTINE Dump( failRC )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( d2Arr )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, "SIZE( d2Arr ) =", SIZE( d2Arr )

            DO i = 1, SIZE( d2Arr )
                IF (.NOT.ALLOCATED( d2Arr( i )%d1%b%b ))&
                            CALL zzrc( (failRC + 10_4 + i) )


                PRINT 10, i, d2Arr( i )%d1%b%b
10              FORMAT(I2,I3)
            END DO


            IF (SIZE( d2Arr ) /= 10) CALL zzrc( (failRC + 1_4) )

        END SUBROUTINE Dump

END PROGRAM varIsArrSectExprIsScalar06
