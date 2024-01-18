!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2009
!*
!*  DESCRIPTION                : miscellaneous (dummy and actual shall defer the
!                               same length type parameter)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (n)
        integer, len :: n

        integer :: i(n)
    end type

    contains

    subroutine test1(a1, i1)
        type (A(*)), allocatable, intent(out) :: a1
        integer, intent(in) :: i1(a1%n)

    end subroutine
end module

use m
    type(A(:)), allocatable :: a1, a2
    integer i1(100)

    allocate (A(100) :: a1)
    call test1 (a1, i1) !<-- illegal

    call test1 (a2, i1) !<-- illegal
end
