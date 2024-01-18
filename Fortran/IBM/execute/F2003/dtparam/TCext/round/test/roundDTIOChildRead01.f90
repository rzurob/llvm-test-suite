! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/round/test/roundDTIOChildRead01.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 14/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with DTIO
!*
!*  DESCRIPTION                : test differnt round= mode specified
!*                               in parent's OPEN, and carried over to
!*                               child during read.
!* ===================================================================

  module m
     type dt(n1,k1,k2,k3)    ! (20,4,8,16)
        integer, kind :: k1,k2,k3
        integer, len  :: n1

        real(k1)         r4
        real(k2)         r8
        real(k3)         r16
        contains

        procedure::readFormat
        generic :: read(formatted) => readFormat
     end type

     contains

     subroutine readFormat(dtv, unit, iotype, v_list, iostat, iomsg)
          class(dt(*,4,8,16)), intent(inout) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) ::iostat
          character(*), intent(inout)::iomsg
          character(*), intent(in)  ::iotype
          integer, intent(in)::v_list(:)

          character(20) r_mode

          read(unit, fmt='(f6.5, f14.13, f25.24)', iostat=iostat,      &
            & iomsg=iomsg) dtv%r4, dtv%r8, dtv%r16

          if(iostat /= 0) return

          if(transfer(dtv%r4, 0) .ne. 1067450788_4) error stop 1_4

          if(transfer(dtv%r8, 0_8) .ne. 4608308547941528973_8) then
              error stop 2_4
          endif

          if(dtv%r16 .ne. z'3FF400355F73E1C2BC97B092CC929AFF') then
             error stop 3_4
          endif

          inquire(unit, round=r_mode)

          if(r_mode .ne. "UP") error stop 2_4

      end subroutine
  end module

  program roundDTIOChildRead01

    use m

    type(dt(20,4,8,16)) :: dt1

    open(unit=3, file='roundDTIOChildRead01.dat', action="read",round="up")
    read(3,*) dt1

    close(3)

  end program roundDTIOChildRead01
