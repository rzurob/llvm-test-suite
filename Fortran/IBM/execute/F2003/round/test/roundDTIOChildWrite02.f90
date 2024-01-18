!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 14/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with WRITE statment
!*                               in DTIO
!*
!*  DESCRIPTION                : test multiple child write statements with 
!*                               ROUND= specifier. Subsequent write statment
!*                               should not be affected by previous statement. 
!* ===================================================================

  module m
     type dt

        real r
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

          write(unit, fmt='(f7.5)', iostat=iostat, iomsg=iomsg,     &
            & round="up") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
             & round="down") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
            & round="zero") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
            & round="nearest") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
             & round="compatible") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
              & round="processor_defined") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          ! round is omitted in this statement

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg) dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

      end subroutine
  end module

  program roundDTIOChildWrite02
    use m

    type(dt) :: dt1=dt(1.250058651)

    open(unit=3, file='roundDTIOChildWrite02.out')
    write(3,*) dt1

  end program roundDTIOChildWrite02 
