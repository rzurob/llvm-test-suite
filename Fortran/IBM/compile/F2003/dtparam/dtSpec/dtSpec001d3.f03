! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: if () is used in derived-type-spec, then
!                               type-param-value-list MUST be present.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, kind :: k = 4
        integer, len :: n = 10

        real(k) :: data (n)
    end type
end module

program dtSpec001d3
use m
    !! the following syntax errors should be diagnosed
    class(A()), allocatable :: a1
    type(A()), pointer :: a2
end
