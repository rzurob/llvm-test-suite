! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321967)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(13) c
    character(1) a1
    real r1(3), r2(3)

    logical(4), external :: precision_r4

    c = '1.2, 1.2, 1.2'

    write (2, '(2a)') c, ' '

    write (2, '(2a)') c, ',    '

    rewind 2

    read (2, '(3F5.1)', advance='no', size=i1) r1


    read (2, '(/,3F5.1)', advance='no', size=i2) r2

    do i = 1, 3
        if (.not. precision_r4(r1(i), 1.2_4)) error stop 1_4

        if (.not. precision_r4(r2(i), 1.2_4)) error stop 2_4
    end do

    if ((i1 /= 12) .or. (i2 /= 11)) error stop 3_4
    end