!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/26/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 312148)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        function t(n)
            real, dimension(3) :: t
        end function
    end interface

    interface A
        subroutine x(n, p)
            procedure(real) :: p
        end subroutine 

        subroutine y (n, p)
        import t
            procedure(t) p
        end subroutine
    end interface
end module

program main
use m
    procedure(t) :: square
    external f

    integer n

    n = 2

    call x(n, f)

    if (n /= 3) error stop 1_4

    call y (n, square)

    if (n /= 50) error stop 2_4
end


real function f(n)
    f = n*1.6
end function

subroutine x(n, p)
    procedure(real) :: p

    n = p(n)
end subroutine


subroutine y (n, p)
use m, only: t
    procedure(t) :: p

    n = sum (p(n))
end subroutine

function square (n)
    real square(3)

    square = (/n, n+1, n+2/)**2
end function
