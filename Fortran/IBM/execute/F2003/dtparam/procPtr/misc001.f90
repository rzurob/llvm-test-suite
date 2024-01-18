!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2009
!*
!*  DESCRIPTION                : miscellaneous (procedure declaration stmt)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (k,n)
        integer, kind :: k
        integer, len :: n

        real(k) data(n)
    end type

    abstract interface
        function genX (r1)
        import
            real(8), intent(in) :: r1(:)
            type(X(8, size(r1))) genX
        end function
    end interface
end module

use m, only: X, genX
    implicit none

    type(X(8, 200)), target :: t1

    type(X(8,:)), pointer :: p1
    integer i
    logical(4), external :: precision_r8

    procedure(genX) g1

    p1 => t1

    p1 = g1 (1.0d0*[(i, i = 1, 200)])

    do i = 1, 200
        if (.not. precision_r8(t1%data(i), 1.25d0*i)) error stop 2_4
    end do
end

function g1 (r1)
use m, only: X
    real(8), intent(in) :: r1(:)
    type(X(8, size(r1))) g1

    g1%data(:) = 1.25d0*r1(:)
end function
