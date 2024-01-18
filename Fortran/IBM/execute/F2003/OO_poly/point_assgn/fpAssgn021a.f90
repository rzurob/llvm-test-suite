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
!*                               bindings; array is poly array)
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
end module

program fpAssgn021
use m
use m1
    class (base), pointer :: b_ptr
    class (child), allocatable, target :: c_allo(:)

    integer*4 :: i

    allocate (c_allo(10))

    do i = 1, size(c_allo)
        b_ptr => c_allo(i)
        c_allo%name = 'c_allo'

        call b_ptr%assgnID (i)
    end do

    do i = 1, size(c_allo)
        b_ptr => c_allo(i)

        if (b_ptr%id /= b_ptr%getID()) error stop 1_4
        if (b_ptr%getID() /= i) error stop 2_4

        call b_ptr%print
    end do

    deallocate (c_allo)
end
