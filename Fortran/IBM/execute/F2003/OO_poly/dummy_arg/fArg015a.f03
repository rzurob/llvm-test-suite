! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2005
!*
!*  DESCRIPTION                : dummy_arg (explicit-shape array and default
!                               initialization for INTENT(OUT))
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

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine resetVal (b, x)
        class(base), intent(out) :: b(2)
        class (*), intent(out) :: x(3)
    end subroutine
end module

program fArg015a
use m
    class (base), allocatable :: b1(:,:)
    class (*), pointer :: x1(:)

    allocate (b1(3,2), source=reshape((/(child(i, 'test 01'), i=1,6)/), (/3,2/)))
    allocate (x1(8), source=(/(child(i*10, 'test 02'), i=1,8)/))

    call resetVal (b1(:,2), x1(::2))

    do j = 1, 2
        do i = 1, 3
            call b1(i,j)%print
        end do
    end do

    select type (x1)
        class is (base)
            do i = 1, 8
                call x1(i)%print
            end do
        class default
            error stop 1_4
    end select

    !! 2nd test
    deallocate (b1, x1)

    allocate (b1(5,2), source=reshape((/(child(i, 'test 01'), i=1,10)/), (/5,2/)))

    call resetVal (b1(2,:), b1(3:,1))

    do j = 1, 2
        do i = 1, 5
            call b1(i,j)%print
        end do
    end do
end
