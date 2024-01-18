! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/d327459.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/31/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327459)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: i
    end type
end module

use m
    type(base(:,4)), allocatable, target :: b1(:)
    type(base(:,4)), pointer :: b2(:)

    b1 = [(base(20,4)(i), i = 1, 10)]

    b2 => b1(::2)

    b1 = b2

    do i = 1, 5
        if (b1(i)%i /= 2*i-1) error stop 1_4
    end do
end
