! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The assumed type parameters for the
!                               components that are of parameterized derived
!                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = 0.0
    end type

    type :: base (k, ka, na, n)
        integer, kind :: k, ka
        integer, len :: n, na

        integer(k) :: id(n) = 0
        type (A(ka, na)) :: data
    end type
end module

module baseProc
use m
    contains

    subroutine addVal2Base4_8 (b, r1, i1)
        type (base(4,8,*,*)), intent(inout) :: b
        integer(4), intent(in) :: i1(b%n)
        real(8), intent(in) :: r1(b%na)

        b%id = b%id + i1
        b%data%data = b%data%data + r1
    end subroutine

    subroutine addBase8_8 (b1, b2)
        type (base(8,8,*,*)), intent(inout) :: b1
        type (base(8,8, b1%na, b1%n)), intent(in) :: b2

        b1%id = b1%id + b2%id
        b1%data%data = b1%data%data + b2%data%data
    end subroutine
end module

program typeParamOrder004a1
use baseProc
    class (base(4,8,:,:)), allocatable :: b1
    type (base(8,8,:,:)), pointer :: b2(:)

    logical(4), external :: precision_r8

    integer(4) i1(20)
    double precision d1(30)

    allocate (base(4,8, 20, 15) :: b1)
    allocate (base(8,8, 35, 45) :: b2(10))

    b1%id = (/(i, i=1, 15)/)
    b1%data%data = log ((/(i*1.0d0, i=1,20)/))

    i1 = (/(i*100, i=1, 20)/)
    d1 = log ((/(i*1.0d0, i=30,1,-1)/))

    do i = 1, 10
        b2(i)%id = (/(i*100+j, j=1,45)/)

        b2(i)%data%data = (/(i*1.0d2+j*1.0d0, j=1,35)/)
    end do

    call addVal2Base4_8 (b1, d1, i1)

    do i = 2, 10
        call addBase8_8 (b2(1), b2(i))
    end do

    !! verify
    if (any (b1%id /= (/(j*101, j=1,15)/))) error stop 1_4

    do i = 1, 20
        if (.not. precision_r8(b1%data%data(i), log(i*(31-i)*1.0d0))) &
                error stop 2_4
    end do

    if (any(b2(1)%id /= 5500+10*(/(j, j=1,45)/))) error stop 3_4

    do i = 1, 35
        if (.not. precision_r8(b2(1)%data%data(i), 5.5d3+1.0d1*i)) &
                error stop 4_4
    end do
end
