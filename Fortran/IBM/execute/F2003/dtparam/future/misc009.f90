!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 317408)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base8_100! (k, n)
        real(8) :: data(100)
    end type

    type base4_210! (k, n)
        real(4) :: data(210)
    end type

    type (base8_100), parameter :: baseConst(10) = &
        (/(base8_100((/(i*1.0d3+j, j=1,100)/)), i=1, 10)/)
end module

module n
use m
    type container4_210_32! (k,n,m)
        type (base4_210) :: data(32) = (/(base4_210 &
            ((/(baseConst(i-(i-1)/10*10)%data, j=1,210/100), &
            baseConst(i-(i-1)/10*10)%data(:mod(210,100))/)), i=1,32)/)
    end type
end module

program dtparamDefInit006
use m
use n
    type(container4_210_32) :: co1

    logical(4), external :: precision_r4

!    co1 = container(4,210,32)()

    !! verify co1
    do i = 1, 32
        do j = 1, 210
            if (.not. precision_r4(co1%data(i)%data(j), &
                (i-(i-1)/10*10)*1.e3+(j-(j-1)/100*100))) error stop 1_4
        end do
    end do
end
