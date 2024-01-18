!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C478; part 2: If the type-parameter does
!                               not have a default value, then a value should be
!                               supplied in type-parameter-spec.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtSpec003d1
use m
    class(base(4)), allocatable :: b1       !<-- illegal
    class(base(n=:)), pointer :: b2         !<-- illegal
end
