! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/scalarExpr/scalarFuncResultExpr03.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar Result of the same Type
!*                               from a FUNCTION.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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

MODULE mModule

    TYPE :: tType(N1,K1)    ! (20,4)
        INTEGER, KIND              :: K1
        INTEGER, LEN               :: N1
        INTEGER(K1)                :: i
        TYPE(tType(:,K1)), POINTER :: p
    END TYPE tType

    TYPE(tType(:,4)), ALLOCATABLE, TARGET :: tTypeAlloc( : )

END MODULE mModule


PROGRAM scalarFuncResultExpr03
    USE mModule

    INTERFACE
        FUNCTION ScalarExpr( t )
            USE mModule
            TYPE(tType(*,4)) :: t
            TYPE(tType(20,4)) :: ScalarExpr
        END FUNCTION ScalarExpr
    END INTERFACE

    TYPE(tType(20,4)), TARGET :: tTypeScalar = tType(20,4)(-1,NULL( ))


    ALLOCATE(tTypeAlloc( 10 ), SOURCE=[ (tType(20,4)(i,NULL( )), i = 0, 9) ])

    DO i = 1, 10
        tTypeAlloc( i )%p => tTypeAlloc( (11 - i) )
    END DO

    CALL Dump([ (i, i = 0, 9) ], [ (i, i = 9, 0, -1) ], 10, 10_4)


    tTypeAlloc = ScalarExpr( tTypeAlloc( 7 ) )
    CALL Dump([ (6, i = 1, 10) ], [ (6, i = 1, 10) ], 10, 50_4)

    tTypeScalar%p => tTypeScalar
    tTypeAlloc = ScalarExpr( tTypeScalar )
    CALL Dump([ (-1, i = 1, 10) ], [ (-1, i = 1, 10) ], 10, 100_4)

    tTypeAlloc = ScalarExpr( tType(20,4)(10,tTypeScalar%p) )
    CALL Dump([ (10, i = 1, 10) ], [ (-1, i = 1, 10) ], 10, 150_4)


    CONTAINS


        SUBROUTINE Dump(ip, pp, s, failRC)
            INTEGER :: ip( : )
            INTEGER :: pp( : )
            INTEGER :: s
            INTEGER(4) :: failRC

            INTEGER(4) :: i


            IF (.NOT. ALLOCATED( tTypeAlloc )) CALL zzrc( failRC )

            DO i = 1, SIZE( tTypeAlloc )
                IF (.NOT. ASSOCIATED( tTypeAlloc( i )%p ))&
                            CALL zzrc( (failRC + 10_4 + i) )
            END DO


            PRINT 10, failRC, SIZE( tTypeAlloc ), s,&
                      tTypeAlloc( : )%i, (tTypeAlloc( i )%p%i, i = 1, 10)
10          FORMAT(I3," S=",I2," (",I2,") (",I2,9(I3),") (",I2,9(I3),")")


            IF (SIZE( tTypeAlloc ) /= s) CALL zzrc( (failRC + 1_4) )

            DO i = 1, SIZE( tTypeAlloc )
                IF (tTypeAlloc( i )%i /= ip( i ))&
                        CALL zzrc( (failRC + 20_4 + i) )
                IF (tTypeAlloc( i )%p%i /= pp( i ))&
                        CALL zzrc( (failRC + 30_4 + i) )
            END DO

        END SUBROUTINE Dump

END PROGRAM scalarFuncResultExpr03


FUNCTION ScalarExpr( t )
    USE mModule

    TYPE(tType(*,4)) :: t

    TYPE(tType(20,4)) :: ScalarExpr


    ScalarExpr = t

END FUNCTION ScalarExpr