!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that slashes, TR or X has no effect on
!                               decimal edit mode in READ set by DC/DP or by
!                               decimal= in the write statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program decEditDesc011
    logical(4), external :: precision_r4
    real(4) :: r1, r2, r3

    open (1, file='decEditDesc011.data', delim='quote')

    write (1, '(a)') 'starting line'


    write (1, '(10e14.6)') (i*1.0, i=1, 10)

    write (1, '(10e14.6)', decimal='COMMA') (i*1.0, i=20, 29)

    rewind (1)

    read (1, '(dc,2/, ss, 70X, e14.6, dp)') r1

    rewind (1)

    read (1, '(/,/, sp, TR28, e14.6, dp)', decimal='comma') r2

    rewind (1)

    read (1, '(dc, 2/, sp, T71, e14.6, dp)', decimal='point') r3

    if (.not. precision_r4(r1, 25.0_4)) error stop 1_4

    if (.not. precision_r4(r2, 22.0_4)) error stop 2_4

    if (.not. precision_r4(r3, 25.0_4)) error stop 3_4
end
