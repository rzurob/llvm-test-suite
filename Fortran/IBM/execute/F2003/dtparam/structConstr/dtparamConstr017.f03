! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor not available for
!                               the derived type that is not accessible; A
!                               solution for the situation in dtparamConstr017d:
!                               use generic name to override the structure
!                               constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, private, extends(base) :: child(l)
        integer, len :: l

        character(l) :: name
    end type

    type (base(8, 12)) :: b1
    type (child(4, 25, 20)) :: c1

    interface base
        module procedure genBaseObj8
    end interface

    interface child
        module procedure genChildObj4
    end interface

    contains

    function genBaseObj8 (d)
        real(8), intent(in) :: d(:)

        type(base(8, size(d))) genBaseObj8

        genBaseObj8%data = d
    end function

    function genChildObj4 (r, name)
        real(4), intent(in) :: r(:)
        character(*), intent(in) :: name

        type(child(4,size(r), len(name))) genChildObj4

        genChildObj4%data = r
        genChildObj4%name = name
    end function
end module

program dtparamConstr017d
use m
    logical(4), external :: precision_r4, precision_r8

    !! structure constructor is not available for the inaccessible types
    !! so use generics to override the structure constructor
    b1 = base((/(i*1.12d0, i=1, 12)/))
    c1 = child((/(1.2*i, i=25,1,-1)/), 'c1 in the main program'(1:20))

    !! verify
    if ((b1%n /= 12) .or. (c1%n /= 25) .or. (c1%l /=20)) error stop 1_4

    do i = 1, 12
        if (.not. precision_r8 (b1%data(i), i*1.12d0)) error stop 2_4
    end do

    do i = 1, 25
        if (.not. precision_r4 (c1%data(i), 1.2*(26-i))) error stop 3_4
    end do
end
