! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd528.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2006
!*
!*  DESCRIPTION                : type bound proc
!                               Mix of specific, generic and procedure pointer
!                               component with the same interface.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i

        procedure(print), pointer :: p1

        contains

        procedure :: printB
        generic :: print => printB
    end type

    abstract interface
        recursive subroutine print(b)
        import
            class (base(*,4)), intent(inout) :: b
        end subroutine
    end interface

    procedure(print) printB
end module

program ftpbnd528
use m
    type (base(20,4)) b1

    procedure(print) printB1

    b1%i = 100
    b1%p1 => printB1

    call b1%print

    if (b1%i /= 0) error stop 1_4

    print *, new_line('a'), 'again'

    call b1%print
end

recursive subroutine printB(b)
use m, only: base, print
    class (base(*,4)), intent(inout) :: b

    if (associated(b%p1)) then
        call b%p1
    end if
end subroutine


recursive subroutine printB1(b)
use m, only: base, print
    class (base(*,4)), intent(inout) :: b

    if (b%i > 0) then
        write (*, '(i10)', advance='no') b%i
        b%i = b%i - 1

        call b%print
    end if
end subroutine