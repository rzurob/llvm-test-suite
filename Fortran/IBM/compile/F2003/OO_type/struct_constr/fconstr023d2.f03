! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (shape conformance
!                               between component and source-expr)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real*4 :: x(2)
    end type

    type, extends(point) :: point3D
        real*4 :: z
    end type
end module


program fconstr023d2
use m
    type (point3D) :: p3_1 = point3D (x = (/1.0, 2.0, 3.0, 4.0/), z = 1.0)
end
