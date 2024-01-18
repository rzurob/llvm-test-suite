!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/15/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324172)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer, allocatable :: i
    end type
end module

use m
    type (A), allocatable :: a1(:)

    a1 = (/(A(null()), i=1,100)/)

    forall (i=10:50)
        a1(i) = A(i)
    end forall

    if (.not. allocated(a1)) error stop 1_4

    if (size(a1) /= 100) error stop 2_4

    do i = 1, 9
        if (allocated(a1(i)%i)) error stop 3_4
    end do

    do i = 51, 100
        if (allocated(a1(i)%i)) error stop 4_4
    end do

    do i = 10, 50
        if (.not. allocated(a1(i)%i)) error stop 5_4

        if (a1(i)%i /= i) error stop 6_4
    end do
end
