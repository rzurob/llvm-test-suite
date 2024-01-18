!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Procedure pointer component with kind type
!                               parameter that is of default value.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k = 8
        integer, len :: n = 55

        real(k) :: data(n)
        procedure(real(k)), nopass, pointer :: val => null()
    end type

    type algorithm (k)
        integer, kind :: k = 8

        procedure(real(k)), nopass, pointer :: func => null()
    end type
end module

program dtparamDefVal013
use m
    type (base), allocatable :: b1(:)
    type (algorithm) :: calc(5)

    real(8), external :: log8, sqrt8, sinh8, asin8, exp8
    real(8) :: d1(5)

    calc = (/algorithm(log8), algorithm(sqrt8), algorithm(sinh8), &
            algorithm(asin8), algorithm(exp8)/)

    allocate (b1(5), source=(/(base((/(i*1.001d2+j, j=1,55)/), calc(i)%func), &
            i=1, 5)/))

    do i = 1, 5
        d1(i) = b1(i)%val(sum(b1(i)%data))
    end do

    write (*, '(5g15.6)') d1
end

real(8) function log8(d1)
    double precision, intent(in) :: d1

    log8 = log(d1)
end function

double precision function sqrt8 (d1)
    double precision, intent(in) :: d1

    sqrt8 = sqrt(d1)
end function

double precision function sinh8 (d1)
    real(8), intent(in) :: d1

    sinh8 = sinh(d1 - int(d1, 8))
end function

real(8) function asin8 (d1)
    real(8), intent(in) :: d1

    asin8 = asin(d1 - int(d1, 8))
end function

function exp8 (d1)
    real(8), intent(in) :: d1

    real(8) exp8

    exp8 = exp(log(d1))
end function
