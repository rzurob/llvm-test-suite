! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Assumed type parameter in
!                               argument-association: polymorphic dummy-arg.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: ids(n)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child (m)
        integer, len :: m

        real :: data(m)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(n=*)), intent(in) :: b

        write (*, '(5i10)') b%ids
    end subroutine

    subroutine printChild (b)
        class(child(*,*)), intent(in) :: b

        call printBase(b%base)

        write (*, '(5f10.2)') b%data
    end subroutine
end module

program assumedparam003
use m
    class(base(:)), allocatable :: b1(:)

    allocate (base(6) :: b1(10))

    do i = 1, 10
        b1(i)%ids = (/(i*10 + j, j = 1, 6)/)
    end do

    call printBaseArray (b1)

    deallocate(b1)

    allocate(child(12, 8) :: b1(5))

    do i = 1, 5
        b1(i)%ids = (/(i*100 + j, j= 12, 1, -1)/)
    end do

    select type (b1)
        class is (child(*,*))
            do i = 1, 5
                b1(i)%data = (/(j*1.2, j = 1, 8)/)
            end do
        class default
            error stop 1_4
    end select

    call printBaseArray (b1)

    contains

    subroutine printBaseArray (b)
        class(base(*)), intent(in) :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do
    end subroutine
end
