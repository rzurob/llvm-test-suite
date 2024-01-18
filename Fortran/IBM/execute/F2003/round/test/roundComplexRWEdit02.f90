!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*
!*  DESCRIPTION                :
!*                 test  ROUND mode while file connected to the program
!*                 for direct access for complex data type.
!* ===================================================================

  program roundComplexRWEdit02

    implicit none

    integer i
    character(18) :: r_mode(6)
    complex(8) w1, w2, r1(6), r2(6)
    real(8) vreal1(6), vimag1(6), vreal2(6), vimag2(6)

    integer, parameter::unit = 2

    w1 = (1.250058651037551D0, -1.250058651037551D0)
    w2 = (3.141592653589551D0, 2.718281828457551D0)

    open(unit, access="direct", form="formatted", recl=100, status="scratch")

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    vreal1 = (/ z'3FF4003D80049C48', z'3FF4003D80049C47',        &
                z'3FF4003D80049C47', z'3FF4003D80049C47',        &
                z'3FF4003D80049C47', z'3FF4003D80049C47' /)

    vimag1 = (/ z'BFF4003D80049C47', z'BFF4003D80049C48',        &
                z'BFF4003D80049C47', z'BFF4003D80049C47',        &
                z'BFF4003D80049C47', z'BFF4003D80049C47'/)

    vreal2 = (/ z'400921FB54442AF5', z'400921FB54442AF4',        &
                z'400921FB54442AF4', z'400921FB54442AF5',        &
                z'400921FB54442AF5', z'400921FB54442AF5'/)

    vimag2 = (/ z'4005BF0A8B144A43', z'4005BF0A8B144A42',        &
                z'4005BF0A8B144A42', z'4005BF0A8B144A42',        &
                z'4005BF0A8B144A42', z'4005BF0A8B144A42'/)

    do i =1, 6
       write(unit,'(RP, DC, 2f17.14, 2f16.14)', rec=i) w1, w2
    end do

    do i =1,6
       read(unit,'(DC, 2f17.14, 2f16.14)',round=r_mode(i), rec=i) &
         & r1(i), r2(i)
    end do

    do i = 1, 6
       if(vreal1(i) .ne. dreal(r1(i))) call zzrc(i)
    end do

    do i = 1, 6
       if(vimag1(i) .ne. dimag(r1(i))) call zzrc(i+10_4)
    end do

    do i = 1,  6
       if(vreal2(i) .ne. dreal(r2(i))) call zzrc(i+20_4)
    end do

    do i = 1,  6
       if(vimag2(i) .ne. dimag(r2(i))) call zzrc(i+30_4)
    end do

    close(unit)

  end program roundComplexRWEdit02
