! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: diagnostic case: Use of procedure target
!                               for the data pointer.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type
end module

program dtparamConstr039
use m
    type container
        class(*), pointer :: data
    end type

    type (container) :: co1

    procedure(type(point(8))), pointer :: genCoord

    co1 = container(genCoord)
end
