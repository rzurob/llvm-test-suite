! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2005
!*
!*  DESCRIPTION                : allocate statement (named constants used in
!                               the source-expr; use array sections of named
!                               constants of derived type)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8) r1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        write (*, '(f12.3)') b%r1
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        write (*, '(f12.3,tr1,a)') b%r1, b%name
    end subroutine
end module

program falloc005a29
use m
    type (child), parameter :: c_const (10) = &
            (/(child(i*1.1_8, 'xlftest'), i=10,1, -1)/)

    class(base), allocatable :: b1, b2(:)

    class (base), pointer :: b3(:)


    allocate (b1, source=c_const(3))

    allocate (b2(5), source=c_const(:5)%base)

    allocate (b3(2), source=c_const(7:10:3))

    !! verify results
    call b1%print

    do i = 1, 5
        call b2(i)%print
    end do

    call b3(1)%print
    call b3(2)%print
end
