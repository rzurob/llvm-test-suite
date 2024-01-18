! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd529a.f
! opt variations: -ql

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/31/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound
!                               Case: defect 318098.  Type bound and procedure
!                               pointer component share the same procedure.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = -1

        procedure(print), pointer :: p1

        contains

        procedure :: printB
        generic :: print => printB
    end type

    abstract interface
        recursive subroutine print(b)
        import
            class (base(4)), intent(inout) :: b
        end subroutine
    end interface

    procedure(print) printB
end module

program ftpbnd529a
use m
    type (base(4)) b1

    b1 = base(4)(10, printB)

    call b1%print
end

recursive subroutine printB(b)
use m, only: base, print
    class (base(4)), intent(inout) :: b

    if (b%i <= 0) return

    print *, b%i

    b%i = b%i - 1

    if (associated(b%p1)) then
        call b%p1
    end if
end subroutine

