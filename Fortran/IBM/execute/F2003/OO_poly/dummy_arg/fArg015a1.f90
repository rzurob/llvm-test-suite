!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/25/2005
!*
!*  DESCRIPTION                : argument association (explicit-shape array and
!                               default initializations for INTENT(OUT)
!                               dummy-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: print
    end type

    contains

    subroutine printBase(b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine


    subroutine print (b)
        class(child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine resetVal (b)
        class(base), intent(out) :: b(3)
    end subroutine
end module


program fArg015a1
use m
    class (base), allocatable, target :: b1(:,:)
    type (base), pointer :: b2(:,:), b3(:)

    allocate (b1(3, 5), source=child(1, 'test'))

    b2 => b1
    b3 => b1(2,::2)

    call resetVal (b2(1,:))
    call resetVal (b3)

    do j = 1, 5
        do i = 1, 3
            call b1(i, j)%print
        end do
    end do
end
