!#######################################################################
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
    type base
        type (base), pointer :: x => null()
    end type
end module

program fconstr026d2
use m
    type (base) :: b1
    b1 = base (x = base(x = base(x = base())))
end

