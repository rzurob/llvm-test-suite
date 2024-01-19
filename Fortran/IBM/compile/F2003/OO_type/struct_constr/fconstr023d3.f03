! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (shape comformance test
!                               on source-expr for a component)
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
    type base
        real*4 :: x(2, 2)
    end type

    type, extends(base) :: child
        real*4 :: z
    end type
end module


program fconstr023d3
use m
    type (child) :: c1 = child (x = (/1.0, 2.0, 3.0, 4.0/), z = 1.0)
end
