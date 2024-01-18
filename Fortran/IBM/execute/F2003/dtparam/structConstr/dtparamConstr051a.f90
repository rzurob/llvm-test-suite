! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/15/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use the defined binary operator as the
!                               data-source.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realArray (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type base (k)
        integer, kind :: k

        type(realArray(k,:)), allocatable :: data
    end type

    interface operator (**)
        module procedure rArrayPower4
    end interface

    contains

    function rArrayPower4 (ra1, n)
        class(realArray(4,*)), intent(in) :: ra1
        integer, intent(in) :: n

        type(realArray(4, ra1%n)) rArrayPower4

        rArrayPower4 = realArray (4, ra1%n)(ra1%data**n)
    end function
end module

program dtparamConstr051a
use m
    type(base(4)) b1
    logical(4), external :: precision_r4

    b1 = base(4)(realArray(4,100)((/(i, i=1,100)/))**2)

    if (.not. allocated(b1%data)) error stop 1_4

    if (b1%data%n /= 100) error stop 2_4

    do i = 1, 100
        if (.not. precision_r4 (b1%data%data(i), (i*1.0_4)**2)) error stop 3_4
    end do
end
