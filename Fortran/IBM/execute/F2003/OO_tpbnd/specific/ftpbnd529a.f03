! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2006
!*
!*  DESCRIPTION                : specific type bound
!                               Case: defect 318098.  Type bound and procedure
!                               pointer component share the same procedure.
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
end module

program ftpbnd529a
use m
    type (base) b1

    b1 = base(10, printB)

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
