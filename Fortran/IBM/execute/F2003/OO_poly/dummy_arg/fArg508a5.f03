! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) attribute
!                               will default-initialize actual-arg; implict
!                               interface for non-poly dummy-arg of
!                               explicit-shape; actual args are poly-array)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m(:)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program fArg508a5
use m
    class (base), pointer :: b_ptr(:)

    allocate (b_ptr(0:10), source=child (100, 'b_ptr'))

    allocate (b1_m(11), source=child(200,'b1_m'))

    call reset (b_ptr(::2))
    call reset (b1_m)

    do i = 1, 3
        call b_ptr(2*i-2)%print
        call b_ptr(2*i-1)%print

        call b1_m(i)%print
    end do

    print *, 'second test'

    call b1_m(4)%print
    call b_ptr(6)%print
    call b_ptr(7)%print

    deallocate (b_ptr, b1_m)
end

subroutine reset (b)
use m
    type (base), intent(out) :: b(3)
end subroutine
