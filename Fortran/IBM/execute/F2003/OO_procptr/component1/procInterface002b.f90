!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
        procedure(), nopass, pointer :: spp
    end type

    contains

    subroutine sub1(i)
        integer, intent(in) :: i(2,3)
        print *, "sub1", i
    end subroutine

    subroutine sub2(b, c)
        type(Base), intent(in) :: b
        type(Base), intent(in) :: c(5)
        print *, "sub2", b%i, c%i
    end subroutine
end module

program procInterface002b
use m
    implicit type(Base) (b)

    b1 = Base(5, null())

    b1%spp => sub1
    call b1%spp((/(i,i=1,6)/))

    b1%spp => sub2
    call b1%spp(Base(3,null()), (/(Base(i,null()),i=1,5)/))
end
