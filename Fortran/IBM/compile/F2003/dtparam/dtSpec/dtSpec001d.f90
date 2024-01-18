!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derive-type-spec.)
!                               Case: C477: Type-parameter-spec-list can ONLY be
!                               specified if the derived type is parameterized.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer k
    end type
end module

program dtSpec001d
use m
    type (A(4)) :: a1                   !<-- illegal
    type (A(k=:)), allocatable :: a2    !<-- illegal
end
