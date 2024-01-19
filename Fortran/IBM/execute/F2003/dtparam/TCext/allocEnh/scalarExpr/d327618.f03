! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/F2003/allocEnh/scalarExpr/d327618.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

!*  ===================================================================
!*
!*                               an Array
!*
!*  DATE                       : November  2, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: IMPDO: Derived Type with ALLOCATABLE
!*                               Component not Allocated in Array Constructor
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Reduced Code below initializes an Array of Derived Type (with a Component
!*  of Derived Type that has an ALLOCATABLE Component) using an Array
!*  Constructor with an Implied-DO.
!*
!*  The Array Initialization through the Implied-DO FAILs to Allocate
!*  the ALLOCATABLE Component.  An Array Constructor with the Structure
!*  Constructor (but no Implied-DO) will successfully Allocate the
!*  ALLOCATABLE Component.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

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
    i = 1
    d2Arr = (/ tD1(4,20)(tB(20,4)(i)) /)
    d2Arr1 = (/ tD1(4,20)(tB(20,4)(1)) /)

    PRINT *, ALLOCATED( d2Scalar%b%b ),&
             ALLOCATED( d2Arr( 1 )%b%b ), ALLOCATED( d2Arr1( 1 )%b%b )

END PROGRAM varIsArrSectExprIsScalar06
