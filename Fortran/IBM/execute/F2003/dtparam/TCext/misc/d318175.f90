! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/misc/d318175.f
! opt variations: -ql -qreuse=none

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
!*  DATE                       : 04/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 318175)
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

    type, extends(base) :: child    ! (4)
        real(k1) :: r = 1.0

        contains
        procedure :: printB => printC
    end type

    interface
        recursive subroutine printC (b)
        import
            class(child(4)), intent(inout) :: b
        end subroutine
    end interface
end module

program ftpbnd
use m
    procedure(print) printB1

    class(base(4)), allocatable :: b1

    allocate (b1, source=child(4)(10, printB1, 1.35))

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


recursive subroutine printC (b)
use m, only: base, child
    class(child(4)), intent(inout) :: b

    call b%base%print

    write (*, '(f10.2)') b%r
end subroutine


recursive subroutine printB1 (b)
use m
    class (base(4)), intent(inout) :: b

    call b%print
end subroutine
