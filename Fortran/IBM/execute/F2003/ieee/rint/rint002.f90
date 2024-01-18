! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/fxieee.presh rint002
! %COMPOPTS: -qstrict -qfloat=rrm:nofold -qxlf90=signedzero
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : March 13, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with array elements.
!*  SECONDARY FUNCTIONS TESTED : IEEE_SET_ROUNDING_MODE
!*                               IEEE_GET_ROUNDING_MODE
!*
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase contains :
!* 1.Test rounding to nereast
!* 2.Test rounding to zero
!* 3.Test rounding to +INF
!* 4.Test rounding to -INF
!*
!234567890123456789012345678901234567890123456789012345678901234567890
         program fxieee05

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         implicit none
         integer :: i
         real*4 :: xr(10), res(10)
         real*8 :: xr_8(10), res_8(10)
         real*16 :: xr_16(10), res_16(10)
         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
         type(ieee_round_type), parameter :: rt_up = IEEE_UP
         type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
         type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)

             do i = 2,9
                 xr(i) = 0.0
                 xr_8(i) = 0.0
                 xr_16(i) = 0.0
             enddo

!  test nereast

             call ieee_get_status(status_value)

             call ieee_set_rounding_mode(rt_nearest)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_nearest) error stop 1

            !  test real*4
             xr(1) = 2345.89
             xr(10) = -34367.3
             res = ieee_rint(xr)
             if (res(1) /= 2346.0 )error stop 2
             if (res(10) /= -34367.0 ) error stop 3

             !  test real*8
             xr_8(1) = 2345.89_8
             xr_8(10) = -34367.3_8
             res_8 = ieee_rint(xr_8)
             if (res_8(1) /= 2346.0 ) error stop 4
             if (res_8(10) /= -34367.0 ) error stop 5


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo

             !  test real*16
             xr_16(1) = 2345.89_16
             xr_16(10) = -34367.3_16
             res_16 = ieee_rint(xr_16)
             if (res_16(1) /= 2346.0 ) error stop 6
             if (res_16(10) /= -34367.0 ) error stop 7

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


! test  to zero

             call ieee_set_status(status_value)

             call ieee_set_rounding_mode(rt_20)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_20) error stop 8

             ! test real*4
             xr(1) = 2345.89
             xr(10) = -34367.9
             res = ieee_rint(xr)
             if (res(1) /= 2345.0 ) error stop 9
             if (res(10) /= -34367.0 ) error stop 10

             !  test real*8
             xr_8(1) = 2345.89_8
             xr_8(10) = -34367.63_8
             res_8 = ieee_rint(xr_8)
             if (res_8(1) /= 2345.0 ) error stop 11
             if (res_8(10) /= -34367.0 ) error stop 12

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 60
             enddo

             !  test real*16
             xr_16(1) = 2345.89_16
             xr_16(10) = -34367.93_16
             res_16 = ieee_rint(xr_16)
             if (res_16(1) /= 2345.0 ) error stop 13
             if (res_16(10) /= -34367.0 ) error stop 14

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 60
             enddo


! test +INF
             call ieee_set_status(status_value)

             call ieee_set_rounding_mode(rt_up)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_up) error stop 15

             ! test real*4
             xr(1) = 2345.12
             xr(10) = -34367.9
             res = ieee_rint(xr)
             if (res(1) /= 2346.0 ) error stop 16
             if (res(10) /= -34367.0 ) error stop 17

             !  test real*8
             xr_8(1) = 2345.29_8
             xr_8(10) = -34367.63_8
             res_8 = ieee_rint(xr_8)
             if (res_8(1) /= 2346.0 ) error stop 18
             if (res_8(10) /= -34367.0 ) error stop 19

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 70
             enddo

             !  test real*16
             xr_16(1) = 2345.19_16
             xr_16(10) = -34367.93_16
             res_16 = ieee_rint(xr_16)
             if (res_16(1) /= 2346.0 ) error stop 20
             if (res_16(10) /= -34367.0 ) error stop 21

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 70
             enddo


! test to -INF
             call ieee_set_status(status_value)

             call ieee_set_rounding_mode(rt_down)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_down) error stop 22

             ! test resl*4
             xr(1) = 2345.912
             xr(10) = -34367.29
             res = ieee_rint(xr)
             if (res(1) /= 2345.0 ) error stop 23
             if (res(10) /= -34368.0 ) error stop 24

             !  test real*8
             xr_8(1) = 2345.829_8
             xr_8(10) = -34367.163_8
             res_8 = ieee_rint(xr_8)
             if (res_8(1) /= 2345.0 ) error stop 25
             if (res_8(10) /= -34368.0 ) error stop 26

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 80
             enddo

             !  test real*16
             xr_16(1) = 2345.819_16
             xr_16(10) = -34367.293_16
             res_16 = ieee_rint(xr_16)
             if (res_16(1) /= 2345.0 ) error stop 27
             if (res_16(10) /= -34368.0 ) error stop 28

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 80
             enddo


         end program
