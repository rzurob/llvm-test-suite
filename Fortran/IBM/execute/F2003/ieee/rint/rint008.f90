! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/fxieee.presh rint008
! %COMPOPTS: -qstrict -qfloat=rrm -qieee -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Modules with IEEE_RINT  with reals.
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_ROUNDING_MODE
!*                               IEEE_SET_ROUNDING_MODE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modus
         contains

         subroutine bar1(x,resx)
            use ieee_arithmetic
            use ieee_exceptions
            use constants_for_ieee
            integer :: i
      real*4 :: x, resx
      type(ieee_round_type) :: rtype
      type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
      type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
      type(ieee_round_type), parameter :: rt_up = IEEE_UP
      type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
      type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
      type(ieee_status_type) :: status_value
            logical :: flag_values(5)


!  test nereast

            call ieee_set_rounding_mode(rt_nearest)
      call ieee_get_rounding_mode(rtype)
      if (rtype /= rt_nearest)  error stop 10
      call ieee_get_status(status_value)

            !  test real*4
            resx = ieee_rint(x)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 100
             enddo

            i = int(resx)
            if (resx .ne. 2.0) call zzrc(i)
            call ieee_set_status(status_value)

         end subroutine bar1

         subroutine bar2(y,resy)

            use ieee_arithmetic
            use ieee_exceptions
            use constants_for_ieee

            integer :: i
      real*8 :: y, resy
      type(ieee_round_type) :: rtype
      type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
      type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
      type(ieee_round_type), parameter :: rt_up = IEEE_UP
      type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
      type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
      type(ieee_status_type) :: status_value
            logical :: flag_values(5)

!  test +INF
      call ieee_set_rounding_mode(rt_up)
      call ieee_get_rounding_mode(rtype)
      if (rtype /= rt_up)  error stop 20
      call ieee_get_status(status_value)

            !  test real*8
            resy = ieee_rint(y)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo

            j = int(resy)
            if (resy .ne. 1.0) call zzrc(j)
            call ieee_set_status(status_value)

         end subroutine bar2

         subroutine bar3(z,resz)

            use ieee_arithmetic
            use ieee_exceptions
            use constants_for_ieee

      integer :: i
      real*16 :: z, resz
      type(ieee_round_type) :: rtype
      type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
      type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
      type(ieee_round_type), parameter :: rt_up = IEEE_UP
      type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
      type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
      type(ieee_status_type) :: status_value
            logical :: flag_values(5)

!  test -INF
            call ieee_set_rounding_mode(rt_down)
      call ieee_get_rounding_mode(rtype)
      if (rtype /= rt_down)  error stop 30
      call ieee_get_status(status_value)

             !  test real*16
             resz = ieee_rint(z)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo

             k = int(resz)
             if (resz .ne. 3.0) call zzrc(k)
            call ieee_set_status(status_value)

         end subroutine bar3

      end module modus

      program foo

      use modus
      real*4 :: a, resa
      real*8 :: b, resb
      real*16 :: c, resc

      a = 1.6_4
      b = 0.2_8
      c = 3.9_16
      call bar1(a, resa)
      call bar2(b, resb)
      call bar3(c, resc)

       end
