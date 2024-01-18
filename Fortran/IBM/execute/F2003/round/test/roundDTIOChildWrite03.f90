!*  ===================================================================
!*
!*  DATE                       : 14/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with DTIO
!*
!*  DESCRIPTION                : test differnt round= mode specified
!*                               in parent's OPEN, and carried over to
!*                               child using complex data type.
!* ===================================================================

  module m
     type dt

        complex c4
        complex(8) c8
        complex(16) c16
        contains

        procedure::writeFormat
        generic :: write(formatted) => writeFormat
     end type

     contains

     subroutine writeFormat(dtv, unit, iotype, v_list, iostat, iomsg)
          class(dt), intent(in) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) ::iostat
          character(*), intent(inout)::iomsg
          character(*), intent(in)  ::iotype
          integer, intent(in)::v_list(:)

          character(20) r_mode

          write(unit, fmt='(2f10.5, 1x, 2f10.5, 1x, 2f31.28)', iostat=iostat, &
           & iomsg=iomsg) dtv%c4, dtv%c8, dtv%c16

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

      end subroutine
  end module

  program roundDTIOChildWrite03

    use m

    type(dt) :: dt1=dt((1.250058651, -1.250058651),                &
                       (1.250058651D0, -1.250058651D0),            &
                  (1.2500586510356702302302302345691Q0,            &
                   -1.2500586510356702302302302345691Q0))

    open(unit=3, file='roundDTIOChildWrite03.out', round="up")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out', round="down")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out', round="zero")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out', round="nearest")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out', round="processor_defined")
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out')
    write(3,*) dt1

    open(unit=3, file='roundDTIOChildWrite03.out', round="compatible")
    write(3,*) dt1

  end program roundDTIOChildWrite03
