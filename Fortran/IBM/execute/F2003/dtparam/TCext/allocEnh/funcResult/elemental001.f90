! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/elemental001.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/08/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that elemental function result used as expr
!                               for the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: i

        contains

        procedure :: common => commonBaseVal
    end type

    contains

    elemental type(base(20,4)) function commonBaseVal (b1, b2)
        class(base(*,4)), intent(in) :: b1, b2

        if ((.not. allocated(b1%i)) .or. (.not. allocated(b2%i))) return

        if (b1%i == b2%i) commonBaseVal%i = b1%i
    end function
end module

program elemental001
use m
    type(base(:,4)), allocatable :: b1(:), b3

    b1 = (/(base(20,4)(null()), i=1,3), (base(20,4)(i), i=1,3), (base(20,4)(null()), i=1,3)/)

    b3 = base(20,4)(2)

    b1 = b3%common(b1)

    do i = 1, 4
        if (allocated(b1(i)%i)) error stop 1_4

        if (allocated(b1(i+5)%i)) error stop 2_4
    end do

    if (.not. allocated(b1(5)%i)) error stop 3_4

    if (b1(5)%i /= 2) error stop 4_4
end
