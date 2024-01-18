!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is a module function. Non-poly.
!                              Intrinsic or derived type, scalar or
!                              array.
!
!                              The dummy arguments of the associated
!                              function are arrays. This test case
!                              involves sequence association where the
!                              size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
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

    type Container
        procedure(integer), nopass, pointer :: pp1
        procedure(type(Base)), nopass, pointer :: pp2
    end type

    contains

    integer function func1(b)
        integer, intent(in) :: b(3:*)
        print *, "func1", b(:8)
        func1 = size(b(:8))
    end function

    type(Base) function func2(b)
        type(Base), intent(in) :: b(5,3)
        print *, "func2", b
        func2 = Base(size(b))
    end function
end module

program procInterface001b
use m
    type(Container) :: c1
    integer rv1
    type(Base) rv2
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/),(/-1/),(/2,1/))

    c1%pp1 => func1
    c1%pp2 => func2

    rv1 = c1%pp1(i1)
    print *, "Func1", rv1

    rv2 = c1%pp2(reshape((/(Base(i),i=11,25)/),(/4,3,2/), &
     (/Base(-1),Base(-2)/), (/2,3,1/)))
    print *, "Func2", rv2
end
