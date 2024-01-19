!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with real*8 in READ statement
!*
!*  DESCRIPTION                :
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!*           test derived type component as input item with different
!*           round mode.
!* ===================================================================

  module m

    type base
      real*8 rd1(6)
      real*8 rd2(6)
    end type
  end module m

  program roundR8ReadEdit03

    use m

    implicit none

    character(18) :: r_mode(6)
    integer i
    integer ios
    integer*8 vf_rd1(6), vf_rd2(6)
    type(base) rd

    integer, parameter::unit_r = 2

    ios = 0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    rd%rd1 = 0.0
    rd%rd2 = 0.0
    vf_rd1 =(/4608308547941528973_8, 4608308547941528972_8,            &
              4608308547941528972_8, 4608308547941528972_8,            &
              4608308547941528972_8, 4608308547941528972_8 /)

    vf_rd2 =(/-4615063488913246836_8,-4615063488913246835_8,           &
              -4615063488913246836_8, -4615063488913246836_8,          &
              -4615063488913246836_8, -4615063488913246836_8 /)

    open(unit_r, file='roundR8ReadEdit03.dat', action='read')

    do i = 1, 2

       read(unit_r, '(f14.13, f15.13)', iostat=ios, round=r_mode(i))   &
       rd%rd1(i), rd%rd2(i)

       if(ios /= 0) call zzrc(i+10_4)

       if(transfer(rd%rd1(i), 0_8) .ne. vf_rd1(i)) call zzrc(i+20_4)
       if(transfer(rd%rd2(i), 0_8) .ne. vf_rd2(i)) call zzrc(i+30_4)

       rewind(unit_r)

    end do

    close(unit_r)

  end program roundR8ReadEdit03
