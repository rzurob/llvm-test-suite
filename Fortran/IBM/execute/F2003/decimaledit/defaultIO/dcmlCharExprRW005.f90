! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               For list-directed READ for complex data, the end
!                               of the record may ocurr between the real part
!                               and the separator or between the separator and
!                               the imaginary part; test the preconnect files.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer i
        complex(8) :: cx(10)
    end type
end module

program dcmlCharExprRW005
use m
    type (base), pointer :: b1(:)

    double precision d1(100)

    logical(4), external :: precision_x6

    integer counter

    allocate (b1(5))

    d1 = (/(i, i=1,100)/)

    write (1, 100, decimal='COMMA') (i, d1(20*i-19:20*i), i=1,5)

    rewind 1

    read (1, *, decimal='COMMA') b1

    !! verify the results of b1
    counter = 1

    do i = 1, 5
        if (b1(i)%i /= i) error stop 1_4

        do j = 1, 10
            if (.not. precision_x6 (b1(i)%cx(j), cmplx(counter, counter+1,8)))&
                    error stop 2_4

            counter = counter + 2
        end do
    end do

100 format (5(1x, i5, ";", 10( " ( ", g25.15, /, ";", /, g25.15, " ) ")))
end
