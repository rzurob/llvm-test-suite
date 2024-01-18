! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The default type parameters for the
!                               parameterized component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, kind :: k = 4
        integer, len  :: n

        real(k) :: data(n)
    end type

    type base (k, n, l)
        integer, kind :: k = 8
        integer, len :: n = 100, l=20

        integer(k) :: id
        type (A(k, n)) :: data1
        type (A(n=n)) :: data2
        type (A(n=2)) :: data3(n)
        character(l) :: name
    end type
end module

program dtparamDefVal002
use m
    logical(4), external :: precision_r4, precision_r8

    type (base) :: b1(10)

    b1(1)%id = 2_8**35
    b1(1)%name = 'xlftest'

    !! the following 2 stmts are using a real type with greater precision to
    ! ensure the accuracy od the computation
    b1(1)%data1%data = exp(log((/(i*1.0q0, i=1, 100)/)))
    b1(1)%data2%data = log(exp((/(i*1.0d0, i=1, 100)/)))

    b1(1)%data3%data(1) = cshift(b1(1)%data1%data, 50)
    b1(1)%data3%data(2) = cshift(b1(1)%data2%data, -50)

    b1(9) = b1(1)

    !! verify values of b1(9)
    if ((b1(9)%name /= 'xlftest') .or. (b1(9)%id/2**29 /= 64)) error stop 1_4

    do i = 1, 100
        if (.not. precision_r8(b1(9)%data1%data(i), i*1.0d0)) error stop 2_4

        if (.not. precision_r4(b1(9)%data2%data(i), i*1.0e0)) error stop 3_4

        if (i <= 50) then
            if (.not. precision_r4(b1(9)%data3(i)%data(1), (i+50)*1.0e0)) &
                    error stop 4_4

            if (.not. precision_r4(b1(9)%data3(i)%data(2), (i+50)*1.0e0)) &
                    error stop 5_4
        else
            if (.not. precision_r4(b1(9)%data3(i)%data(1), (i-50)*1.0e0)) &
                    error stop 6_4

            if (.not. precision_r4(b1(9)%data3(i)%data(2), (i-50)*1.0e0)) &
                    error stop 7_4
        end if
    end do
end
