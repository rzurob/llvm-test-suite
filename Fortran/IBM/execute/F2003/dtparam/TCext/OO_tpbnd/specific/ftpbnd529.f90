! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd529.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2006
!*
!*  DESCRIPTION                : specific type bound
!                               Case: defect 318098.  Specific type bound and
!                               the procedure pointer component share the same
!                               interface.
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

        procedure :: print => printB
    end type

    abstract interface
        recursive subroutine print(b)
        import
            class (base(*,4)), intent(inout) :: b
        end subroutine
    end interface

    procedure(print) printB
end module

program ftpbnd529
use m
    type (base(20,4)) b1

    b1%i = 100
    b1%p1 => printB

    call b1%print

    if (associated(b1%p1)) error stop 1_4
end

recursive subroutine printB(b)
use m, only: base, print
    class (base(*,4)), intent(inout) :: b
    procedure(print), pointer :: localPrint

    print *, b%i

    if (associated(b%p1)) then
        localPrint => b%p1

        nullify (b%p1)
        call localPrint(b)
    end if
end subroutine

