!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*
!*  DESCRIPTION                :
!*                 test  ROUND mode while in the asynchronous mode.
!* ===================================================================

  program roundRealRWEdit03

    implicit none

    integer i, idvar
    character(18) :: r_mode(6)
    real*8 w1, w2, w3, w4, r1(6), r2(6), r3(6), r4(6)
    real*8  vw1(6), vw2(6), vw3(6), vw4(6)

    integer, parameter::unit = 2

    w1 = 1.250058651037551D0
    w2 = -1.250058651037551D0
    w3 = 3.141592653589551D0
    w4 = 2.718281828457551D0

    open(unit, access="direct", asynchronous='yes', form="formatted", &
      & recl=34, status="scratch")

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    vw1 = (/ z'3FF4003D80049C48', z'3FF4003D80049C47',            &
             z'3FF4003D80049C47', z'3FF4003D80049C47',            &
             z'3FF4003D80049C47', z'3FF4003D80049C47' /)

    vw2 = (/ z'BFF4003D80049C47', z'BFF4003D80049C48',            &
             z'BFF4003D80049C47', z'BFF4003D80049C47',            &
             z'BFF4003D80049C47', z'BFF4003D80049C47'/)

    vw3 = (/ z'400921FB54442AF5', z'400921FB54442AF4',            &
             z'400921FB54442AF4', z'400921FB54442AF5',            &
             z'400921FB54442AF5', z'400921FB54442AF5'/)

    vw4 = (/ z'4005BF0A8B144A43', z'4005BF0A8B144A42',            &
             z'4005BF0A8B144A42', z'4005BF0A8B144A42',            &
             z'4005BF0A8B144A42', z'4005BF0A8B144A42'/)

    do i =1, 6
       write(unit,'(RP, f16.14, f17.14)', rec=i) w1, w2
    end do

    do i = 7, 12
       write(unit,'(RN, f16.14, f16.14)',id=idvar, rec=i) w3, w4
    end do

    ! while the second write is being performed, read data written by
    ! the first read statement

    do i =1,6
       read(unit,'(f16.14, f18.14)',round=r_mode(i), rec=i) r1(i), &
        & r2(i)
    end do

    wait(id=idvar)

    do i=1,6
       read(unit,'(f16.14, f16.14)',round=r_mode(i), rec=i+6) r3(i),&
        & r4(i)
    end do

    do i = 1, 6
       if(vw1(i) .ne. r1(i)) call zzrc(i)
    end do

    do i = 1, 6
       if(vw2(i) .ne. r2(i)) call zzrc(i+10_4)
    end do

    do i = 1,  6
       if(vw3(i) .ne. r3(i)) call zzrc(i+20_4)
    end do

    do i = 1,  6
       if(vw4(i) .ne. r4(i)) call zzrc(i+30_4)
    end do

    close(unit)

  end program roundRealRWEdit03