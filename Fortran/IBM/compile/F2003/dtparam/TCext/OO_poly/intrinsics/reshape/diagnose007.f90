! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/reshape/diagnose007.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/08/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case: ORDER shall have the same shape as SHAPE.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type
end module

program diagnose007
use m
    class(Base(:,4)), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(20,4)(i),i=1,15)/))

    print *, reshape(b1, (/3,6/), (/Base(20,4)(1)/), (/1/))
end
