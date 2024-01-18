!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-10-01
!*
!*  DESCRIPTION                : defect 368969. case 3: ICE in ASTI
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM varIsArrSectExprIsScalar06

    TYPE :: tB(N1,K1)    ! (20,4)
        INTEGER, KIND            :: K1
        INTEGER, LEN             :: N1
        INTEGER(K1), ALLOCATABLE :: b
    END TYPE tB

    TYPE :: tD1(K2,N2)    ! (4,20)
        INTEGER, KIND   :: K2
        INTEGER, LEN    :: N2
        TYPE(tB(N2,K2)) :: b
    END TYPE tD1

    TYPE(tD1(4,20)) :: d2Scalar
    TYPE(tD1(4,20)) :: d2Arr( 1 )
    TYPE(tD1(4,20)) :: d2Arr1( 1 )

    d2Scalar = tD1(4,20)(tB(20,4)(99))
    d2Arr = (/ (tD1(4,20)(tB(20,4)(i)), i = 1, 1) /)
    d2Arr1 = (/ tD1(4,20)(tB(20,4)(1)) /)

    PRINT *, ALLOCATED( d2Scalar%b%b ),&
             ALLOCATED( d2Arr( 1 )%b%b ), ALLOCATED( d2Arr1( 1 )%b%b )

END PROGRAM varIsArrSectExprIsScalar06
