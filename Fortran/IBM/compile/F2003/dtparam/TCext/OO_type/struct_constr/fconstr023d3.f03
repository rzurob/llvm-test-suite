! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr023d3.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x(2, 2)
    end type

    type, extends(base) :: child(k2)    ! (4,4)
        integer, kind :: k2
        real(k2)      :: z
    end type
end module


program fconstr023d3
use m
    type (child(4,4)) :: c1 = child(4,4) (x = (/1.0, 2.0, 3.0, 4.0/), z = 1.0)
end
