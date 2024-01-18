! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/round/test/roundDTIOChildWrite02b.f
! opt variations: -qnol

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
!*  DESCRIPTION                : test round mode takes precedence over
!*                               IEEE rounding mode during WRITE in DTIO.
!* ===================================================================

  module m

     use ieee_arithmetic
     type dt(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1

        real(k1)         r
        contains
 
        procedure::writeFormat
        generic :: write(formatted) => writeFormat
     end type

     contains

     subroutine writeFormat(dtv, unit, iotype, v_list, iostat, iomsg)
          class(dt(*,4)), intent(in) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) ::iostat
          character(*), intent(inout)::iomsg
          character(*), intent(in)  ::iotype
          integer, intent(in)::v_list(:)

          character(20) r_mode

          call ieee_set_rounding_mode(ieee_up)

          write(unit, fmt='(f7.5)', iostat=iostat, iomsg=iomsg,   & 
              & round="down") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          call ieee_set_rounding_mode(ieee_down)

          write(unit, fmt='(RU, 2x, f7.5)', iostat=iostat, iomsg=iomsg) &
                & dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          call ieee_set_rounding_mode(ieee_to_zero) 

          write(unit, fmt='(2x, f7.5)', iostat=iostat, iomsg=iomsg, &
               & round="up") dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          call ieee_set_rounding_mode(ieee_nearest)

          write(unit,fmt='(2x, RU, f7.5)',iostat=iostat,iomsg=iomsg) &
                & dtv%r

          if(iostat /= 0) return

          inquire(unit, round=r_mode)

          write(unit, fmt='(20a)') r_mode

          write(unit, fmt='(2x, RU, f7.5)', iostat=iostat,iomsg=iomsg, &
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

  program roundDTIOChildWrite02b
    use m

    type(dt(20,4)) :: dt1=dt(20,4)(1.250058651)

    open(unit=3, file='roundDTIOChildWrite02b.out')
    write(3,*) dt1

  end program roundDTIOChildWrite02b 
