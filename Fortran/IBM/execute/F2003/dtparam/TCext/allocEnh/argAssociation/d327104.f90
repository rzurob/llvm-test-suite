! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/d327104.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/24/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327104)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: data
    end type
end module

use m
    type(base(:,4)), allocatable :: b1(:)

    b1 = [base(20,4) :: ]

    do i = 1, 3
        b1 = [ b1 , base(20,4)(i) ]
    end do

    if (.not. allocated(b1)) error stop 1_4

    if (size(b1) /= 3) error stop 2_4

    do i = 1, 3
        if (b1(i)%data /= i) error stop 3_4
    end do
end
