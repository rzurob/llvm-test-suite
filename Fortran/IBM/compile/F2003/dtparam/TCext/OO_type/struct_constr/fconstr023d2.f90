! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr023d2.f
! SCCS ID Information
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
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x(2)
    end type

    type, extends(point) :: point3D(k2)    ! (4,4)
        integer, kind :: k2
        real(k2)      :: z
    end type
end module


program fconstr023d2
use m
    type (point3D(4,4)) :: p3_1 = point3D(4,4) (x = (/1.0, 2.0, 3.0, 4.0/), z = 1.0)
end
