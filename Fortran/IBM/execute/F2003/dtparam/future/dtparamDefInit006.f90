!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of the named constants for the default
!                               initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type (base(8,100)), parameter :: baseConst(10) = &
        (/(base(8,100)((/(i*1.0d3+j, j=1,100)/)), i=1, 10)/)
end module

module n1
use m1
    type container (k,n,m)
        integer, kind :: k, n, m

        type (base(k,n)) :: data(m) = (/(base(k,n) &
            ((/(baseConst(i-(i-1)/10*10)%data, j=1,n/100), &
            baseConst(i-(i-1)/10*10)%data(:mod(n,100))/)), i=1,m)/)
    end type
end module

program dtparamDefInit006
use m1, only: base
use n1, only: container
    type(container(4,210,32)) :: co1

    logical(4), external :: precision_r4

    co1 = container(4,210,32)()

    !! verify co1
    do i = 1, 32
        do j = 1, 210
            if (.not. precision_r4(co1%data(i)%data(j), &
                (i-(i-1)/10*10)*1.e3+(j-(j-1)/100*100))) error stop 1_4
        end do
    end do
end
