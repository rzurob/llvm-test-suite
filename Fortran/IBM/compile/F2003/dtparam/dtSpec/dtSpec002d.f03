! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Type-name shall be the accessible derived
!                               type. C476
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A10
        integer(4) :: data(10)
    end type
end module

program dtSpec002d
use m
    type (A(10)) :: a1(10)              !<-- illegal
    type (A(:)), allocatable :: a2(:)   !<-- illegal
end