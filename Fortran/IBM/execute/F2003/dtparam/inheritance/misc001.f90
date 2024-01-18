!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2006
!*
!*  DESCRIPTION                : miscellanous (defect 313465)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point! (k)

        integer(4) x, y
        procedure(type(point)), nopass, pointer :: genPoint => null()
    end type

end module

program dtparamExtends017
use m
    type (Point) cp1(2)

    type (point) pv

    procedure(type(point)) genPoint

    cp1%x = (/1, 2/)
    cp1%y = (/3, 4/)


    cp1(1)%genPoint => genPoint


    pv = cp1(1)%genPoint (cp1(1)%x, cp1(1)%y)

    !! verify the results
    if ((pv%x /= 1) .or. (pv%y /= 3)) error stop 1_4
    if (associated(pv%genPoint)) error stop 2_4

end

type (point) function genPoint (i1, i2)
use m
    integer(4), intent(in) :: i1, i2

    genPoint%x = i1
    genPoint%y = i2
end function

