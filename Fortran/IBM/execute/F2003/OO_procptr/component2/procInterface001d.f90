!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is an intrinsic function.
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

    type, extends(Base) :: Child
        integer j
    end type

    type Container
        procedure(real), pointer, nopass :: pp1
        procedure(func2), pointer, nopass :: pp2
    end type

    contains

    type(Child) function func2(r, p)
        real, intent(in) :: r
        procedure(real), pointer, intent(in) :: p
        func2 = Child(int(p(r)),-int(p(r)))
    end function
end module

program procInterface001d
use m
    implicit type(Container) (c)

    c1%pp1 => sin
    c1%pp2 => func2

    print *, c1%pp2(3.1415926/2, c1%pp1)
end
