!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/25/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the value separator in default READ
!                               and WRITE for complex are correctly set even if
!                               the list items are of NaN or INF.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program valueSeparator005
use ieee_arithmetic
    real(8), allocatable :: d1(:), d2(:,:)

    complex(8), pointer :: cx1(:,:), cx2(:)

    character(:), allocatable :: string

    logical(4), external :: precision_r8, precision_x6


10  format(dc, 12(" (", e22.15, " ; ", e22.15, " );"))

    open (1, file='valueSeparator005.data', access='stream', form='formatted')

    allocate (d1(10))

    d1 = (/(dsqrt(i*1.0d0), i=1,10)/)

    write (1, '(3(d22.15, ";", 1x))', decimal='comma', pos=1) d1(1:4), &
            ieee_value(1.0d0, IEEE_NEGATIVE_INF), d1(5:6), &
            ieee_value(-1.0d0, IEEE_SIGNALING_NAN), d1(7), &
            ieee_value(2.0d1, IEEE_QUIET_NAN), d1(8:10)


    string = repeat(' ', 800)

    allocate (cx1(2,5))

    cx1 = cmplx(reshape((/(i, i=-10,-1)/), (/2,5/)), &
        reshape((/(i**2, i=-10,-1)/), (/2,5/)), 8)

    write (string, 10) cx1(:,:3), cmplx(ieee_value(1.0d0, ieee_positive_inf), &
        ieee_value(1.0d0, ieee_negative_inf), 8), &
        cmplx(ieee_value(1.0d0, ieee_signaling_nan), &
        ieee_value(1.0d0,ieee_quiet_nan), 8), cx1(:,4:5)


    !! now read in data from both external and internal files
    allocate (d2(2,7), cx2(12))

    rewind 1

    read (1, *, decimal='comma') d2(:,1:2), d2(2,6), d2(:,3), d2(1,7), &
        d2(1,4), d2(2,7), d2(2,4), d2(:,5)


    read (string, *, decimal='coMMA') cx2(1:6), cx2(11:12), cx2(7:10)


    !! verify d2 and cx2
    k = 1

    do j = 1, 5
        do i = 1, 2
            if (.not. precision_r8(d2(i,j), sqrt(k*1.0d0))) error stop 1_4

            k = k + 1
        end do
    end do

    if ((.not. ieee_is_negative(d2(2,6))) .or. &
        ieee_is_finite(d2(2,6))) error stop 2_4

    if (.not. ieee_is_nan(d2(1,7))) error stop 3_4

    if (.not. ieee_is_nan(d2(2,7))) error stop 4_4



    do i = 1, 10
        if (.not. precision_x6(cx2(i), cmplx(i-11, (i-11)**2,8))) &
                error stop 5_4
    end do

    if (ieee_is_finite(real(cx2(11), 8)) .or. &
        ieee_is_finite(aimag(cx2(11)))) error stop 6_4

    if (ieee_is_negative(real(cx2(11))) .or. &
        (.not. ieee_is_negative(aimag(cx2(11))))) error stop 7_4


    if ((.not. ieee_is_nan(real(cx2(12)))) .or. &
        (.not. ieee_is_nan(aimag(cx2(12))))) error stop 8_4
end

