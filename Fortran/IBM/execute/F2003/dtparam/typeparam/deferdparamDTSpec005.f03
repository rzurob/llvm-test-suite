! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: solve sine function using
!                               Taylor series.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type function (k, n)
        integer, kind :: k = 8
        integer, len :: n
    end type

    type, extends(function) :: mySine
        real(k) :: coef(n)

        logical, private :: ready = .false.
        contains

        procedure :: setup => setupCoef
        procedure :: compute => computeSine
    end type

    contains

    !! use of Taylor series
    subroutine setupCoef (ms)
        class(mySine(8,*)), intent(inout) :: ms
        integer :: sign = -1

        if (ms%ready) return

        do i = 1, ms%n
            ms%coef(i) = sign**(i-1)/factorial(2*i -1)
        end do

        ms%ready = .true.

        contains

        real*8 function factorial (n)
            real(8) array(n)

            array = (/(i, i=1, n)/)

            factorial = product(array)
        end function
    end subroutine

    real(8) function computeSine (ms, x)
        class (mySine(8, *)), intent(in) :: ms
        real(8), intent(in) :: x

        computeSine = 0.0d0

        do i = 1, ms%n
            computeSine = computeSine + ms%coef(i) * x**(2*i -1)
        end do
    end function
end module

program deferdparamDTSpec005
use m
    type(mySine(8, :)), allocatable :: calculator

    real(8), parameter:: pi = 3.14159265358979d0

    n = 1

1   n = n*2

    allocate (mySine(8, n) :: calculator)

    call calculator%setup

    if (abs(calculator%compute(pi)) > 1.0d-10) then
        deallocate (calculator)
        goto 1
    end if

    if (n /= 16) error stop 1_4
end
