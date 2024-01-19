! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that DC/DP has no effect on size= value for
!                               non-advancing read.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program decEditDesc010
    real(4) :: r1(2, 10)
    integer isize1, isize2

    logical(4), external :: precision_r4

    write (1, '(100f12.4)', decimal='cOmma') (i*1.2, i=1, 20)

    write (1, '(22f12.4)', decimal='comma') (i*2.1, i=1, 10)

    rewind 1

    read (1, '(100(dc, sp, f12.4, dp))', advance='no', size=isize1) r1 (1,:)

    read (1, '(dc, 5(ss, f12.4))', advance='no', size=isize2) r1(2,:)

    if ((isize1 /= 120) .or. (isize2 /= 120)) error stop 1_4

    do i = 1, 10
        if (.not. precision_r4 (r1(1,i), i*1.2_4)) error stop 2_4
    end do

    do i = 1, 5
        if (.not. precision_r4 (r1(2,i), (i+10)*1.2_4)) error stop 3_4
    end do

    do i = 6, 10
        if (.not. precision_r4 (r1(2,i), 2.1_4*(i-5))) error stop 4_4
    end do
end
