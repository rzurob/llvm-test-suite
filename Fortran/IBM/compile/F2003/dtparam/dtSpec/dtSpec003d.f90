!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C478; part 1: There shall be at most one
!                               type-parameter-spec for each parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: i(n)
    end type
end module

program dtSpec003d
use m
    class(base(4, k=8, n=:, n=10)), allocatable :: b1  !<-- illegal
end
