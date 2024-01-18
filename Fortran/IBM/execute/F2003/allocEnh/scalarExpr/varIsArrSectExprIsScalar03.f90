!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : varIsArrSectExprIsScalar03 - expr is a
!*                               Scalar and variable is an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section from an Allocated ALLOCATABLE
!*                               Array of Derived Type,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Derived Type
!*
!*  DRIVER STANZA              : xlf2003
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

MODULE mBase

    TYPE :: tBase
        INTEGER, ALLOCATABLE :: bC
    END TYPE tBase

END MODULE mBase

MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived
        INTEGER :: i
    END TYPE tDerived

END MODULE mDerived

MODULE mAlloc
    USE mDerived

    TYPE(tDerived), ALLOCATABLE :: tAlloc( : )

END MODULE mAlloc

PROGRAM varIsArrSectExprIsScalar03
    USE mAlloc

    INTERFACE
        SUBROUTINE Alloc( )
            USE mAlloc
        END SUBROUTINE Alloc

        SUBROUTINE Dump(iA, bCA, failRC)
            USE mAlloc
            INTEGER :: iA( : )
            INTEGER :: bCA( : )
            INTEGER(4) :: failRC
        END SUBROUTINE Dump
    END INTERFACE

    TYPE(tDerived) :: scalarTDerived = tDerived(NULL( ),10)

    CALL Alloc( )
    CALL Dump([ (i, i = 0, 9) ], [ (i, i = 10, 1, -1) ], 10_4)


    tAlloc( 8:9 ) = tAlloc( 3 )
    CALL Dump([ (i, i = 0, 6),2,2,9 ], [ (i, i = 10, 4, -1),8,8,1 ], 50_4)

    tAlloc( 7:9 ) = tAlloc( 7 )
    CALL Dump([ (i, i = 0, 5),6,6,6,9 ], [ (i, i = 10, 5, -1),4,4,4,1 ], 100_4)

    scalarTDerived%bC = -9
    tAlloc( 6:9 ) = scalarTDerived
    CALL Dump([ (i, i = 0, 4),(10, i = 1, 4),9 ],&
              [ (i, i = 10, 6, -1),(-9, i = 1, 4),1 ], 150_4)

END PROGRAM varIsArrSectExprIsScalar03

SUBROUTINE Alloc( )
    USE mAlloc

    ALLOCATE(tAlloc( 10 ), SOURCE=[ (tDerived((10 - i),i), i = 0, 9) ])

END SUBROUTINE Alloc

SUBROUTINE Dump(iA, bCA, failRC)
    USE mAlloc

    INTEGER :: iA( : )
    INTEGER :: bCA( : )
    INTEGER(4) :: failRC

    INTEGER(4) :: i

    IF (.NOT. ALLOCATED( tAlloc )) CALL zzrc( failRC )

    PRINT *
    PRINT *, failRC, SIZE( tAlloc )

    DO i = 1, SIZE( tAlloc )
        IF (.NOT. ALLOCATED( tAlloc( i )%bC )) CALL zzrc( (failRC + i) )

        PRINT *, i, tAlloc( i )%i, iA( i ), tAlloc( i )%bC, bCA( i )

        IF (tAlloc( i )%i /= iA( i ))   CALL zzrc( (failRC + 10_4 + i) )
        IF (tAlloc( i )%bC /= bCA( i )) CALL zzrc( (failRC + 20_4 + i) )
    END DO

    IF (SIZE( tAlloc ) /= 10) CALL zzrc( (failRC + 31_4) )

END SUBROUTINE Dump
