! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/fxieee.presh rint004
! %COMPOPTS: -qstrict -qfloat=rrm:nofold -qfree=f90 -qxlf90=signedzero
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with reals.
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_ROUNDING_MODE
!*                               IEEE_SET_ROUNDING_MODE
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

         program rint_real

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         integer :: i
         real*4 :: xr1, res1, xr2, res2
         real*8 :: xr1_8, res1_8, xr2_8, res2_8
         real*16 :: xr1_16, res1_16, xr2_16, res2_16
         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
         type(ieee_round_type), parameter :: rt_up = IEEE_UP
         type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
         type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)


!  test nereast

            call ieee_get_status(status_value)


            call ieee_set_rounding_mode(rt_nearest)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_nearest)  error stop 1

            !  test real*4
            xr1 = 2345.89
            xr2 = -34367.3
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 2346.0 ) error stop 2
            if (res2 /= -34367.0 ) error stop 3

            !  test real*8
            xr1_8 = 2345.89_8
            xr2_8 = -34367.3_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 2346.0_8 ) error stop 4
            if (res2_8 /= -34367.0_8 ) error stop 5

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 50
            enddo

            !  test real*16
            xr1_16 = 2345.89_16
            xr2_16 = -34367.3_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 2346.0_16 ) error stop 7
            if (res2_16 /= -34367.0_16 ) error stop 8

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 50
            enddo


! test  to zero

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_20)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_20) error stop 9

            ! test real*4
            xr1 = 2345.89
            xr2 = -34367.9
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 2345.0 ) error stop 10
            if (res2 /= -34367.0 ) error stop 11

            !  test real*8
            xr1_8 = 2345.89_8
            xr2_8 = -34367.63_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 2345.0_8 ) error stop 12
            if (res2_8 /= -34367.0_8 ) error stop 13


            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 60
            enddo

            !  test real*16
            xr1_16 = 2345.89_16
            xr2_16 = -34367.93_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 2345.0_16 ) error stop 14
            if (res2_16 /= -34367.0_16 ) error stop 15

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 60
            enddo



! test +INF

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_up)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_up) error stop 16

            ! test real*4
            xr1 = 2345.12
            xr2 = -34367.9
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 2346.0 ) error stop 17
            if (res2 /= -34367.0 ) error stop 18

            !  test real*8
            xr1_8 = 2345.29_8
            xr2_8 = -34367.63_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 2346.0_8 ) error stop 19
            if (res2_8 /= -34367.0_8 ) error stop 20

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 70
            enddo

            !  test real*16
            xr1_16 = 2345.19_16
            xr2_16 = -34367.93_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 2346.0_16 ) error stop 21
            if (res2_16 /= -34367.0_16 ) error stop 22

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

            ! test real*4
            xr1 = 2345.912
            xr2 = -34367.29
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 2345.0 ) error stop 23
            if (res2 /= -34368.0 ) error stop 24

            !  test real*8
            xr1_8 = 2345.829_8
            xr2_8 = -34367.163_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 2345.0_8 ) error stop 25
            if (res2_8 /= -34368.0_8 ) error stop 26

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 70
            enddo

            !  test real*16
            xr1_16 = 2345.819_16
            xr2_16 = -34367.293_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 2345.0_16 ) error stop 27
            if (res2_16 /= -34368.0_16 ) error stop 28

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 80
            enddo

         end program
