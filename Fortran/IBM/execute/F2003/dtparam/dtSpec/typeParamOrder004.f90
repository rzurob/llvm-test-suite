!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/07/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: For derived type components that are of
!                               parameterized derived type; test the parameter
!                               order using the scalar-int-expr.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, len :: n
        integer, kind :: k

        real(k) :: data(n)
    end type

    type base (k, ka, na, l)
        integer, kind :: ka, k
        integer, len :: l, na

        integer(k) :: id
        class(A(ka, na)), allocatable :: data
        character(l) :: name
    end type
end module

program typeParamOrder004
use m
    type (base(4, 8, 15, 20)) :: b1
    class(base(8, 8, 100, 15)), allocatable :: b2(:)

    logical(4), external :: precision_r8

    allocate (b2(10))

    b1%id = 10
    write (b1%name, '(4i5)') -1, -2, -3, -4
    allocate (b1%data, source=A(8,15)(exp((/(i*1.0d0, i=1,15)/))))

    b2%id = 2_8**30* (/(i*10, i=1, 10)/)
    write (b2%name, '(10(3i5,/))') (/(-i, i=1,30)/)

    do i = 1, 10
        allocate (b2(i)%data, &
            source=A(8,100)(exp(sqrt((/(i*1.0d3+j, j=1,100)/)))))
    end do

    !! verify results
    if ((b1%id /= 10) .or. (any(b2%id/2_8**29 /= 20*(/(j, j=1,10)/)))) &
            error stop 1_4

    if (b1%name /= '   -1   -2   -3   -4') error stop 2_4

    if((b2(1)%name/='   -1   -2   -3') .or.(b2(2)%name/='   -4   -5   -6') .or. &
       (b2(3)%name/='   -7   -8   -9') .or.(b2(4)%name/='  -10  -11  -12') .or. &
       (b2(5)%name/='  -13  -14  -15') .or.(b2(6)%name/='  -16  -17  -18') .or. &
       (b2(7)%name/='  -19  -20  -21') .or.(b2(8)%name/='  -22  -23  -24') .or. &
       (b2(9)%name/='  -25  -26  -27').or.(b2(10)%name/='  -28  -29  -30')) &
            error stop 2_4

    do i = 1, 15
        if (.not. precision_r8(b1%data%data(i), exp(i*1.0d0))) error stop 3_4
    end do

    do i = 1, 10
        do j = 1, 100
            if (.not. precision_r8(b2(i)%data%data(j), &
                    exp(sqrt(i*1.0d3+j*1.0d0)))) error stop 4_4
        end do
    end do
end
