!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with real*16 in READ statement
!*
!*  DESCRIPTION                :
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundR16ReadEdit01

    implicit none

    character(18) :: r_mode(6)
    integer i
    integer ios
    real*16 rd1(6), rd2(6)
    real*16 vf_rd1(6), vf_rd2(6)

    integer, parameter::unit_r = 2

    ios = 0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    rd1 = 0.0_16
    rd2 = 0.0_16
    vf_rd1 =(/z'3FF400355F73E1C2BC97B092CC929AFF',                &
              z'3FF400355F73E1C13CA427B699B6B27F',                &
              z'3FF400355F73E1C2BC97B092CC929B00',                &
              z'3FF400355F73E1C2BC97B092CC929B00'/)

    vf_rd2 =(/z'BFF400355F73E1C23C97B092CC929AFF',                &
              z'BFF400355F73E1C1BCA427B699B6B27F',                &
              z'BFF400355F73E1C23C97B092CC929B00',                &
              z'BFF400355F73E1C23C97B092CC929B00'/)

    open(unit_r, file='roundR16ReadEdit01.dat', action='read', round="up")

    do i = 1, 6

       read(unit_r, '(f25.24, f26.24)', iostat=ios, round=r_mode(i))   &
       rd1(i), rd2(i)

       if(ios /= 0) call zzrc(i+10_4)

       if(rd1(i) .ne. vf_rd1(i)) call zzrc(i+20_4)
       if(rd2(i) .ne. vf_rd2(i)) call zzrc(i+30_4)

       rewind(unit_r)

    end do

    close(unit_r)

  end program roundR16ReadEdit01
