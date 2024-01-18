! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/round/test/roundDTIOChildWrite01.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 14/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with DTIO
!*
!*  DESCRIPTION                : test differnt round= mode specified
!*                               in parent's OPEN, and carried over to
!*                               child.
!* ===================================================================

  module m
     type dt(n1,k1,k2,k3)    ! (20,4,8,16)
        integer, kind :: k1,k2,k3
        integer, len  :: n1

        real(k1)         r4
        real(k2)         r8
        real(k3)         r16
        contains

        procedure::writeFormat
        generic :: write(formatted) => writeFormat
     end type

     contains

     subroutine writeFormat(dtv, unit, iotype, v_list, iostat, iomsg)
          class(dt(*,4,8,16)), intent(in) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) ::iostat
          character(*), intent(inout)::iomsg
          character(*), intent(in)  ::iotype
          integer, intent(in)::v_list(:)

          character(20) r_mode

          write(unit, fmt='(f7.5, 1x, f14.12, 1x, f30.28)', iostat=iostat, &
           & iomsg=iomsg) dtv%r4, dtv%r8, dtv%r16

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

      end subroutine
  end module

  program roundDTIOChildWrite01

    use m

    type(dt(20,4,8,16)) :: dt1=dt(20,4,8,16)(1.250058651, 1.25005865103567D0,               &
       & 1.2500586510356702302302302345691Q0)

    open(unit=3, file='roundDTIOChildWrite01.out', round="up")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out', round="down")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out', round="zero")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out', round="nearest")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out', round="processor_defined")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out')
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite01.out', round="compatible")
    write(3,*) dt1

  end program roundDTIOChildWrite01
