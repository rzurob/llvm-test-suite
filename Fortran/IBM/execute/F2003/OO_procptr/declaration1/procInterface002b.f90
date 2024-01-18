!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either a
!                              module function or a module subroutine.
!                              Non-poly. Intrinsic or derived type,
!                              scalar or array.
!
!                              Use implicit typing to match the
!                              procedure pointeer with the target.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    contains

    subroutine sub1(i)
        integer, intent(in) :: i(2,3)
        print *, "sub1", i
    end subroutine

    function func1(b)
        type(Base), intent(in) :: b
        type(Base) :: func1
        func1 = Base(b%i * 2)
    end function
end module

program procInterface002b
use m
    implicit type(Base) (f)

    procedure(), pointer :: spp
    procedure(), pointer :: fpp

    type(Base) :: b1

    spp => sub1
    fpp => func1

    call spp((/(i,i=1,6)/))
    print *, "func1", fpp(Base(5))
end
