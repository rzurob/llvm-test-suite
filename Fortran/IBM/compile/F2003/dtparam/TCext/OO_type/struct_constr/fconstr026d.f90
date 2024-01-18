! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr026d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (unnamed objects from
!*                               structure constructor can not be used to
!*                               initialize the pointer components)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        type (x), pointer :: x1 => null()
    end type

    type x(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type
end module

program fconstr026d
use m

    type (base(4,20)) :: b1
    b1 = base(4,20) (x(4,20)())     !<-- target attribute is required
end

