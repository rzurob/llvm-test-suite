!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: passed-object)
!                               Case: Generic type bound for different kind
!                               parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        real(k) :: val = -1.0

        contains

        generic :: print => print4, print8
        procedure :: print4 => printBase4
        procedure :: print8 => printBase8
    end type

    contains

    subroutine printBase4 (b)
        class(base), intent (in) :: b

        write (*, '(e10.2)') b%val
    end subroutine

    subroutine printBase8 (b)
        class(base(8)), intent (in) :: b

        write (*, '(g15.5)') b%val
    end subroutine
end module

program dtparamPassObj005
use m
    type (base) b1(3)

    class (base(8)), allocatable :: b2(:)

    allocate (b2(10))

    b1(1)%val = 12.3
    b2(1)%val = dcos(1.0d0)

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print

    call b2(1)%print
    call b2(3)%print
end
