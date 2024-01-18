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
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with DTIO 
!*
!*  DESCRIPTION                : test round mode set differntly
!*                               in parent's OPEN, and in child data 
!*                               trasfer during read.  Test complex data
!*                               type when round set to up.
!* ===================================================================

  module m
     type dt

        complex c4
        complex(8) c8
        complex(16) c16  
        contains
 
        procedure::readFormat
        generic :: read(formatted) => readFormat
     end type

     contains

     subroutine readFormat(dtv, unit, iotype, v_list, iostat, iomsg)
          class(dt), intent(inout) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) ::iostat
          character(*), intent(inout)::iomsg
          character(*), intent(in)  ::iotype
          integer, intent(in)::v_list(:)

          character(20) r_mode

          read(unit, fmt='(RU, 2f7.5,2f15.13, 2f26.24)',iostat=iostat,  &
            & iomsg=iomsg) dtv%c4, dtv%c8, dtv%c16

          if(iostat /= 0) return

          if(transfer(real(dtv%c4), 0) .ne. 1067450788) error stop 1_4

          if(transfer(aimag(dtv%c4), 0) .ne. -1080032861) error stop 2_4

          inquire(unit, round=r_mode)

          if(r_mode .ne. "DOWN") error stop 3_4

          if(transfer(dreal(dtv%c8), 0_8) .ne. 4608308547941528973_8) then
               error stop 4_4
          endif

          if(transfer(dimag(dtv%c8), 0_8) .ne. -4615063488913246836_8)then
                error stop 5_4
          endif

          if(qreal(dtv%c16) .ne. z'3FF400355F73E1C2BC97B092CC929AFF') then
                error stop 6_4
          endif

          if(qimag(dtv%c16) .ne. z'BFF400355F73E1C23C97B092CC929AFF') then
                error stop 7_4
          endif

      end subroutine
  end module

  program roundDTIOChildRead02

    use m

    type(dt) :: dt1
    dt1%c4 = (0.0, 0.0)
    dt1%c8 = (0.0D0, 0.0D0)
    dt1%c16 = (0.0Q0, 0.0Q0)

    open(unit=3, file='roundDTIOChildRead02.dat', action="read", round="down")
    read(3,*) dt1
    close(3)

  end program roundDTIOChildRead02 
