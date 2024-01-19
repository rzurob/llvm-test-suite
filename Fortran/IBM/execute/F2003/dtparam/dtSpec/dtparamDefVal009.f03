! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Default type parameter for the procedure
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 100

        real(k) :: data(n)
        procedure(real(k)), pointer, nopass :: p1 => null()
    end type
end module

program dtparamDefVal009
use m
    real(8), external :: returnVal

    class(base), allocatable :: b1(:)

    logical(4), external :: precision_r8

    allocate (b1(10))

    do i = 1, 10
        b1(i)%data = (/(i*1.0d2+j, j=0, 99)/)
        b1(i)%p1 => returnVal
    end do

    !! verify
    do i = 1, 10
        do j = 1, 100
            if (.not. precision_r8 (b1(i)%p1(b1(i), j), i*1.0d2+j-1)) error stop 1_4
        end do
    end do
end

double precision function returnVal (b, i)
use m
    type(base), intent(in) :: b
    integer, intent(in) :: i

    if (i > b%n) error stop 10

    returnVal = b%data(i)
end function
