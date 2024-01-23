! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/basic/nonchar/unallocatedScalarDerived01.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!*  ===================================================================
!*
!*                               Non-CHARACTER Scalar Derived Type
!*
!*  DATE                       : August 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of Derived Type with the
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

PROGRAM unallocatedScalarDerived01

    TYPE tType(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        REAL(K1)      :: r
    END TYPE tType

    TYPE, EXTENDS(tType) :: dType(N2,K2)    ! (20,4,20,4)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
        COMPLEX(K2)   :: c
    END TYPE dType

    TYPE(dType(:,4,:,4)), ALLOCATABLE :: dTypeAlloc
    TYPE(dType(20,4,20,4)) :: dTypeStatic = dType(20,4,20,4)(1.23,(4.56,7.89))


    IF ( ALLOCATED( dTypeAlloc ) ) ERROR STOP 10_4


    dTypeAlloc = dTypeStatic

    IF (.NOT. ALLOCATED( dTypeAlloc )) ERROR STOP 20_4

    PRINT 10, dTypeAlloc%r, dTypeAlloc%c
10  FORMAT('dTypeAlloc%r = "',F4.2,'", dTypeAlloc%c = (',F4.2,',',F4.2,')')

    IF (dTypeAlloc%r /= 1.23) ERROR STOP 30_4
    IF (dTypeAlloc%c /= (4.56,7.89)) ERROR STOP 40_4


END PROGRAM unallocatedScalarDerived01
