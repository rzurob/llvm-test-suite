!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The associated function is an intrinsic
!                              function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    type(Child) function func2(r, p)
        implicit real (p)
        real, intent(in) :: r
        procedure(real), pointer, intent(in) :: p
        func2 = Child(int(p(r)),-int(p(r)))
    end function
end module

module m2
use m1
    implicit real (r), type(Child) (p)

    type Container
        procedure(real), pointer, nopass :: rpp1
        procedure(func2), pointer, nopass :: pp2
    end type
end module

program procInterface002d
use m2
    class(Container), pointer :: c1
    allocate(Container::c1)

    c1%rpp1 => sin
    c1%pp2 => func2

    print *, c1%pp2(3.1415926/2, c1%rpp1)
end
