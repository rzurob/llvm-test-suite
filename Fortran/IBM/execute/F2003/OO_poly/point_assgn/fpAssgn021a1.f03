! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (scalar pointer
!*                               assigned to array elements; components and
!*                               bindings; array is two dimensional poly-array)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 id

        contains

        procedure, non_overridable :: getID => baseID
        procedure, non_overridable :: assgnID => assgnBaseID
        procedure :: print => basePrint
    end type

    class (base), allocatable, target :: b1_m (:)

    contains

    integer*4 function baseID (b)
        class (base), intent(in) :: b

        baseID = b%id
    end function

    subroutine assgnBaseID (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine basePrint (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module


module m1
use m, only : base

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%getID(), b%name
    end subroutine

    character(2) function int2char (i)
        integer*4, intent(in) :: i

        write (int2char, '(i2.2)') i
    end function
end module

program fpAssgn021
use m
use m1
    class (base), pointer :: b_ptr

    class (child), pointer :: c1 (:,:)
    integer*4 :: i, j

    allocate (c1 (10, 20))

    do i = 1, 10
        do j = 1, 20
            b_ptr => c1(i, j)
            c1(i, j)%name = 'c1.'//int2char(i)//'.'//int2char(j)

            call b_ptr%assgnID (i+j)
        end do
    end do

    do i = 1, 10
        do j = 1, 20
            b_ptr => c1(i, j)

            if (b_ptr%id /= b_ptr%getID()) error stop 1_4
            if (b_ptr%id /= i+j) error stop 2_4

            call b_ptr%print
        end do
    end do

    deallocate (c1)
end