!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is a module function.
!                              Non-poly. Intrinsic or derived type,
!                              scalar.
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

    integer function func1(b)
        type(Base), intent(in) :: b
        func1 = b%i * 2
    end function

    type(Base) function func2(b, p)
        type(Base), intent(in) :: b
        procedure(integer), pointer, intent(in) :: p
        if(.NOT. associated(p)) error stop 5_4
        func2 = Base(p(b))
    end function
end module

module n
use m
    type Container
        procedure(integer), nopass, pointer :: pp1
        procedure(func2), nopass, pointer :: pp2
    end type
end module

program procInterface001c
use m
use n
    implicit type(Container) (c)

    c1 = Container(null(), null())

    if(associated(c1%pp1)) error stop 1_4
    if(associated(c1%pp2)) error stop 2_4

    c1%pp1 => func1
    c1%pp2 => func2

    if(.NOT. associated(c1%pp1)) error stop 3_4
    if(.NOT. associated(c1%pp2)) error stop 4_4

    print *, "func2", c1%pp2(Base(5), c1%pp1)
end
