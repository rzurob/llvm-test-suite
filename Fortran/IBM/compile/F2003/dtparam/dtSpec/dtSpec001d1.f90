! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Derived type parameter declared but not
!                               defined in dt-definition.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n)   !<-- n is not defined
        integer :: data
    end type
end module

program dtSpec001d1
use m
    type (A(100)) :: a1
    type (A(:)), allocatable :: a2
end
