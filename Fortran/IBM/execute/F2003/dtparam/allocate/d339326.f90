!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/17/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 339326)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A ( n )
        integer, len :: n

        integer val(n)
    end type
end module

subroutine test (n, a1)
use m
    type(A( n )), allocatable :: a1

    allocate (a1, source=A(n)([(i, i=1,n)]))
end subroutine

use m
    integer, parameter :: nsize = 10
    type(A(nsize)), allocatable :: a1

    abstract interface
        subroutine procInterface (n, a1)
        import
            type(A( n )), allocatable :: a1
        end subroutine
    end interface

    procedure(procInterface) test

    call test (nsize, a1)

    if (.not. allocated(a1)) error stop 1_4

    if (any(a1%val /= [(i, i=1,nsize)])) error stop 2_4
end
