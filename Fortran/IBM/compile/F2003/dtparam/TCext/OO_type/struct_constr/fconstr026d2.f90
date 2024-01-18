! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr026d2.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (source-expr for the
!                               pointer component must have target attribute)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind             :: k1
        integer, len              :: n1
        type(base(k1,:)), pointer :: x => null()
    end type
end module

program fconstr026d2
use m
    type (base(4,20)) :: b1
    b1 = base(4,20) (x = base(4,20)(x = base(4,20)(x = base(4,20)())))
end

