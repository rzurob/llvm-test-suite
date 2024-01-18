! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 314823)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        contains

        generic :: x => x1, x2

        procedure, nopass :: x1
        procedure, nopass :: x2
    end type

    contains

    subroutine x1(r)
        real(4) r

        print *, 'real kind of 4'
        write (*, '(f10.2)') r
    end subroutine

    subroutine x2(r)
        real(8) r

        print *, 'real kind of 8'
        write (*, '(f15.5)') r
    end subroutine
end module

program misc002
use m
    type(A) a1, a2(100)
    call a1%x(10.0)
    call a2%x(1.d-2)
end

