! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/round/test/roundRealRWEdit06.f
! opt variations: -ql

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*                             
!*
!*  DESCRIPTION                : 
!*                 test different ROUND mode during stream access with 
!*                 derived type. Also test using substring as specifier.
!* ===================================================================

  program roundRealRWEdit06

    implicit none

    type dtReal(k1)    ! (8)
        integer, kind :: k1
        real(k1)      :: r(4)
    end type

    type dt(k2)    ! (8)
        integer, kind :: k2
        real(k2)      :: r(4)
    end type

    integer i
    character(18) :: r_mode(6)
    real*8  vw(4), vw1(6), vw2(6), vw3(6), vw4(6)
    type(dtReal(8)) :: dReal
    type(dt(8)) :: dv(6)

    integer, parameter::unit = 2 

    open(unit, access="stream", form="formatted", status="replace",   &
         file="roundRealRWEdit06.dat")

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    vw = (/ z'3FF4003D80049C48', z'BFF4003D80049C47',             &
             z'400921FB54442AF5',z'4005BF0A8B144A43' /) 

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

    write(unit,'(RP, f16.14, f17.14, f16.14, f16.14)', pos=1)     &
     dtReal(8)((/1.250058651037551D0,-1.250058651037551D0,           &
       3.141592653589551D0, 2.718281828457551D0/))
    
    read(unit,'(f16.14,f17.14,f16.14,f16.14)',round='it is up'(7:8),&
           & pos=1) dReal

    do i = 1, 4 
       if(vw(i) .ne. dReal%r(i)) call zzrc(i)
    end do

    do i =1,6
       read(unit,'(f16.14,f17.14,f16.14,f16.14)',round=r_mode(i), &
           & pos=1) dv(i)
    end do

    do i = 1, 6
       if(vw1(i) .ne. dv(i)%r(1)) call zzrc(i+10_4)
    end do

    do i = 1, 6
       if(vw2(i) .ne. dv(i)%r(2)) call zzrc(i+20_4)
    end do

    do i = 1, 6
       if(vw3(i) .ne. dv(i)%r(3)) call zzrc(i+30_4)
    end do

    do i = 1, 6
       if(vw4(i) .ne. dv(i)%r(4)) call zzrc(i+40_4)
    end do

  end program roundRealRWEdit06
