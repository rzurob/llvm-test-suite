! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/15/2006
!*
!*  DESCRIPTION                : miscellaneous (314922)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        contains

        generic :: x => x1, x2, x3  !<-- illegal, x1, x3 not resolvable

        procedure, nopass :: x1
        procedure, nopass :: x2
        procedure :: x3
    end type

    contains

    subroutine x1(r)
        real(4) r

        print *, 'real kind of 4'
    end subroutine

    subroutine x2(r)
        real(8) r

        print *, 'real kind of 8'
    end subroutine

    subroutine x3(a1, r)
        class (A), intent(in) :: a1
        real(4), intent(in) :: r

        print *, r
    end subroutine
end module

program misc002d
use m
    type(A) a1, a2(100)
    call a1%x(10.0)
    call a2%x(1.d-2)
end


