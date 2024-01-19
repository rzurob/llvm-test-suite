! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the COMMA mode for the derived type object
!                               in list-dirceted output and input.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        complex(8) :: cx(3)
    end type

    type B
        integer i
        real r1
        logical j(2)
        type(A) :: a1 = A ((/1.0, 2.0, 3.0/))
    end type
end module

program commaEdit006
use m
    type (A) a1(10)
    type (B) b1(10)

    logical(4) precision_x6, precision_r4

    character(10) :: stream='stream'

    open (9, file='commaEdit006.out', decimal='decimal point is COMMA'(18:22),&
            access=stream, form='formatted')

    write (9, *) (A(cmplx(3.0*i, 3.0+i, kind=8)), i=1, 10)

    write (9, *, decimal = 'POINT') (B(i, i*1.0, mod(i,2) == 1), i=1,10)

    read (9, *, pos=1, decimal='COMMA') a1
    read (9, *, decimal='POINT') b1

    do i = 1, 10
        do j = 1, 3
            if (.not. precision_x6(a1(i)%cx(j), cmplx(3.0*i, 3.0+i, kind=8))) &
                error stop 1_4

            if (.not. precision_x6(b1(i)%a1%cx(j), cmplx(j*1.0, 0.0, 8))) &
                error stop 2_4
        end do

        if (b1(i)%i /= i) error stop 3_4

        if (.not. precision_r4 (b1(i)%r1, i*1.0)) error stop 4_4

        if (any(b1(i)%j .neqv. (mod(i,2) == 1))) error stop 5_4
    end do
end

