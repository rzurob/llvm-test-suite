! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/round/test/roundR16ReadEdit02.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with real*16 in READ statement
!*
!*  DESCRIPTION                :
!*           test derived type extended component as input.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  module m
    type base(k1)    ! (16)
      integer, kind :: k1
      real(k1)         rd1(6)
    end type

    type, extends(base):: child    ! (16)
      real(k1) rd2(6)
    end type

    implicit type(child(16)) (v)

    allocatable  v

  end module m

  program roundR16ReadEdit02

    use m

    character(18) :: r_mode(6)
    integer i
    integer ios
    real*16 rd1_vf(6), rd2_vf(6)

    integer, parameter::unit_r = 2

    allocate(v)

    ios = 0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    v%rd2 = 0.0Q0
    v%base%rd1 = 0.0Q0

    rd1_vf =(/z'3FF400355F73E1C2BC97B092CC929AFF',                &
              z'3FF400355F73E1C13CA427B699B6B27F',                &
              z'3FF400355F73E1C2BC97B092CC929B00',                &
              z'3FF400355F73E1C2BC97B092CC929B00'/)

    rd2_vf =(/z'BFF400355F73E1C23C97B092CC929AFF',                &
              z'BFF400355F73E1C1BCA427B699B6B27F',                &
              z'BFF400355F73E1C23C97B092CC929B00',                &
              z'BFF400355F73E1C23C97B092CC929B00'/)

    open(unit_r, file='roundR16ReadEdit01.dat', action='read', round="up")

    do i = 1, 6

       read(unit_r, '(f25.24, f26.24)', iostat=ios, round=r_mode(i))   &
       v%base%rd1(i), v%rd2(i)

       if(ios /= 0) call zzrc(i+10_4)

       if(v%base%rd1(i) .ne. rd1_vf(i)) call zzrc(i+20_4)
       if(v%rd2(i) .ne. rd2_vf(i)) call zzrc(i+30_4)

       rewind(unit_r)

    end do

    close(unit_r)

  end program roundR16ReadEdit02
