! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318175)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: i = -1

        procedure(print), pointer :: p1
        contains

        procedure :: printB
        generic :: print => printB
    end type

    abstract interface
        recursive subroutine print(b)
        import
            class (base), intent(inout) :: b
        end subroutine
    end interface

    procedure(print) printB

    type, extends(base) :: child
        real :: r = 1.0

        contains
        procedure :: printB => printC
    end type

    interface
        recursive subroutine printC (b)
        import
            class(child), intent(inout) :: b
        end subroutine
    end interface
end module

program ftpbnd
use m
    procedure(print) printB1

    class(base), allocatable :: b1

    allocate (b1, source=child(10, printB1, 1.35))

    call b1%print
end

recursive subroutine printB(b)
use m, only: base, print
    class (base), intent(inout) :: b

    if (b%i <= 0) return

    print *, b%i

    b%i = b%i - 1

    if (associated(b%p1)) then
        call b%p1
    end if
end subroutine


recursive subroutine printC (b)
use m, only: base, child
    class(child), intent(inout) :: b

    call b%base%print

    write (*, '(f10.2)') b%r
end subroutine


recursive subroutine printB1 (b)
use m
    class (base), intent(inout) :: b

    call b%print
end subroutine
