
module m
    real(8) d1(10)

    real(8), allocatable :: d2(:)
end module

subroutine verifyD1D2
use m
    logical(4) precision_r8

    !! verify d1(1:5) and d2(6:10)
    do i = 1, 5
        if (.not. precision_r8(1.0d-1, d1(i))) error stop 1_4

        if (.not. precision_r8(1.0d-1, d2(i+5))) error stop 2_4
    end do

    if (.not. precision_r8(12.3d-1, d1(6))) error stop 3_4
    if (.not. precision_r8(12.3d-1, d2(1))) error stop 4_4

    if (.not. precision_r8(2.125d3, d1(7))) error stop 5_4
    if (.not. precision_r8(2.125d3, d2(2))) error stop 6_4

    if (.not. precision_r8(3.14d-21, d1(8))) error stop 7_4
    if (.not. precision_r8(3.14d-21, d2(3))) error stop 8_4

    if (.not. precision_r8(.120d1, d1(9))) error stop 9_4
    if (.not. precision_r8(.120d1, d2(4))) error stop 10_4

    if (.not. precision_r8(-.152d124, d1(10))) error stop 11_4
    if (.not. precision_r8(-.152d124, d2(5))) error stop 12_4
end subroutine

subroutine allocateD2
use m
    if (.not. allocated(d2)) then
        allocate(d2(10))
    end if
end subroutine

program commaEdit002
    integer unit

    unit = 9

    open (unit, file='commaEdit002.in', decimal='COMMA', form='formatted', &
            access='stream')

    call readD1 (unit)

    call allocateD2

    !! the following line does NOT change decimal edit mode
    open (unit, file='commaEdit002.in', form='formatted', access='stream')

    rewind (unit)

    call readD2 (9)

    call verifyD1D2
end

subroutine readD1 (unit)
use m
    integer, intent(in) :: unit

    read (unit, fmt='(5D15.3)', decimal='COMMA', advance='no') d1(6:10)

    read (unit, *) d1(:5)
end subroutine

subroutine readD2 (unit)
use m
    integer, intent(in) :: unit

    read(unit, fmt='(5G15.3)', advance='no') d2(1:5)

    read (unit, *) d2(6:10)
end subroutine
