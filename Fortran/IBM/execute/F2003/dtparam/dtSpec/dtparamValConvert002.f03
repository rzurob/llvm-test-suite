! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Value conversion from integer(4) to
!                               integer(8); and others
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer(selected_int_kind(2)), kind :: k
        integer(selected_int_kind(12)), len :: n

        real(k) :: data(n)
    end type
end module

program dtparamValConvert002
use m
    class(base(n=:, k=8_8)), allocatable :: b2
    type(base(n=:, k=4)), allocatable :: b3(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (base(n=255_2,k=8_8):: b2)
    allocate (base(4, 1250) :: b3(10))

    b2%data = log((/(i*1.0d0, i=1, 255)/))

    do i = 1, 10
        b3(i)%data = (/(i*1.0e4+j, j=1,1250)/)
    end do

    if (size(b2%data) /= 255) error stop 1_4
    if (size(b3(5)%data) /= 1250) error stop 2_4

    do i = 1, 255
        if (.not. precision_r8(b2%data(i), log(i*1.0d0))) error stop 3_4
    end do

    do i = 1, 10
        do j = 1, 1250
            if (.not. precision_r4(i*1.0e4+j, b3(i)%data(j))) error stop 4_4
        end do
    end do
end
