! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: type parameters used in the declaration of
!                               procedure pointer components.  The
!                               proc-interface is the derived type with type
!                               parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        integer(k) x, y
        procedure(type(point(k))), nopass, pointer :: genPoint => null()
    end type

    type color(k)
        integer, kind :: k

        integer(k) colorValue
    end type

    type, extends(point) :: colorPoint (colorKind)
        integer, kind :: colorKind

        type(color(colorKind)) color
        procedure(type(colorPoint(k, colorKind))), pointer, nopass :: &
                    genColorPoint => null()
    end type

    integer(1) RED, BLUE, YELLOW

    parameter (RED=10, BLUE=11, YELLOW=12)
end module

program dtparamExtends017
use m
    type (colorPoint(k=4, colorKind=1)) cp1(2)

    type (point(4)) pv
    type (colorPoint(4,1)) cpv

    procedure(type(point(4))) genPoint
    procedure(type(colorPoint(4,1))) genColorPoint

    cp1%x = (/1, 2/)
    cp1%y = (/3, 4/)

    cp1%color%colorValue = (/RED, YELLOW/)

    cp1(1)%genPoint => genPoint
    cp1(2)%genColorPoint => genColorPoint


    pv = cp1(1)%genPoint (cp1(1)%x, cp1(1)%y)
    cpv = cp1(2)%genColorPoint (cp1(2)%x, cp1(2)%y, cp1(2)%color%colorValue)

    !! verify the results
    if ((pv%x /= 1) .or. (pv%y /= 3)) error stop 1_4
    if (associated(pv%genPoint)) error stop 2_4

    if ((cpv%x /= 2) .or. (cpv%y /= 4) .or. (cpv%color%colorValue /= YELLOW)) &
            error stop 3_4

    if (associated(cpv%genPoint) .or. associated(cpv%genColorPoint)) &
            error stop 4_4
end


type (point(4)) function genPoint (i1, i2)
use m
    integer(4), intent(in) :: i1, i2

    genPoint%x = i1
    genPoint%y = i2
end function


type(colorPoint(4,1)) function genColorPoint (i1, i2, colorValue)
use m
    integer(4), intent(in) :: i1, i2
    integer(1), intent(in) :: colorValue

    genColorPoint%x = i1
    genColorPoint%y = i2
    genColorPoint%color%colorValue = colorValue
end function
