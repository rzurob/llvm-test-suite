!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has the same name as
!                              interface-name. The return value of
!                              function is array. Deal with sequence
!                              association.
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

    contains

    function func1(b)
        type(Base) :: b(5,2)
        type(Base) :: func1(3,5)
        func1 = reshape(b,(/3,5/),(/Base(-1),Base(-2)/),(/2,1/))
    end function
end module

module m2
use m1
    contains

    function func2(b, p)
        type(Base) :: b(3,3,2)
        ! interface-name is explicit by use association
        procedure(func1), pointer, intent(in) :: p
        type(Base) :: func2(3,5)
        func2 = p(b)
    end function
end module

module m3
use m2
    procedure(func1), pointer :: pp1
    procedure(func2), pointer :: pp2
end module

program interfaceName003b
use m2
use m3
    pp1 => func1
    pp2 => func2

    print *, pp2((/(Base(i),i=1,20)/), pp1)
    print *, shape(pp2((/(Base(i),i=1,20)/), pp1))
end
