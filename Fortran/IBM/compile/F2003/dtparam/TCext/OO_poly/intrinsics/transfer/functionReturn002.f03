! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/transfer/functionReturn002.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case. When return value of transfer is poly,
!*  it shall not be processed by regular IO.
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
end module

program functionReturn002
use m
    type(Base(20,4)) :: b(4)
    class(Base(:,4)), allocatable :: b1

    b = (/(Base(20,4)(i),i=1,4)/)
    allocate(b1, SOURCE=Base(20,4)(8))

    print *, transfer(b, b1)
end
