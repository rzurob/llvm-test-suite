! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with real constants.
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_ROUNDING_MODE
!*                               IEEE_SET_ROUNDING_MODE
!*
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

         program rint_const

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         integer :: i
         real*4, parameter :: pi = 3.1415926, pi_minus = pi*(-1)
         real*4, parameter :: g = 9.8123456, g_minus = g*(-1)

         real*8, parameter :: pi_8 = 3.1415926_8, pi_8minus = pi_8*(-1)
         real*8, parameter :: g_8 = 9.8123456_8, g_8minus = g_8*(-1)

         real*16, parameter :: pi_16 = 3.1415926_16, pi_16minus = pi_16*(-1)
         real*16, parameter :: g_16 = 9.8123456_16, g_16minus = g_16*(-1)

         real*4 :: res1, res2, gres1, gres2
         real*8 :: res1_8, res2_8,gres1_8, gres2_8
         real*16 :: res1_16, res2_16,gres1_16, gres2_16

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
            res1 = ieee_rint(pi)
            res2 = ieee_rint(pi_minus)
            gres1 = ieee_rint(g)
            gres2 = ieee_rint(g_minus)

            if (res1 /= 3.0 ) error stop 2
            if (res2 /= -3.0 ) error stop 3
            if (gres1 /= 10.0 ) error stop 4
            if (gres2 /= -10.0 ) error stop 5


            !  test real*8
            res1_8 = ieee_rint(pi_8)
            res2_8 = ieee_rint(pi_8minus)
            gres1_8 = ieee_rint(g_8)
            gres2_8 = ieee_rint(g_8minus)

            if (res1_8 /= 3.0_8 ) error stop 6
            if (res2_8 /= -3.0_8 ) error stop 7
            if (gres1_8 /= 10.0_8 ) error stop 8
            if (gres2_8 /= -10.0_8 ) error stop 9


            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 50
            enddo

            !  test real*16
            res1_16 = ieee_rint(pi_16)
            res2_16 = ieee_rint(pi_16minus)
            gres1_16 = ieee_rint(g_16)
            gres2_16 = ieee_rint(g_16minus)

            if (res1_16 /= 3.0_16 ) error stop 10
            if (res2_16 /= -3.0_16 ) error stop 11
            if (gres1_16 /= 10.0_16 ) error stop 12
            if (gres2_16 /= -10.0_16 ) error stop 13

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 50
            enddo


! test  to zero

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_20)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_20) error stop 14

            ! test real*4
            res1 = ieee_rint(pi)
            res2 = ieee_rint(pi_minus)
            gres1 = ieee_rint(g)
            gres2 = ieee_rint(g_minus)

            if (res1 /= 3.0 ) error stop 15
            if (res2 /= -3.0 ) error stop 16
            if (gres1 /= 9.0 ) error stop 17
            if (gres2 /= -9.0 ) error stop 18


            !  test real*8
            res1_8 = ieee_rint(pi_8)
            res2_8 = ieee_rint(pi_8minus)
            gres1_8 = ieee_rint(g_8)
            gres2_8 = ieee_rint(g_8minus)

            if (res1_8 /= 3.0_8 ) error stop 19
            if (res2_8 /= -3.0_8 ) error stop 20
            if (gres1_8 /= 9.0_8) error stop 21
            if (gres2_8 /= -9.0_8 ) error stop 22

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 60
            enddo

            !  test real*16
            res1_16 = ieee_rint(pi_16)
            res2_16 = ieee_rint(pi_16minus)
            gres1_16 = ieee_rint(g_16)
            gres2_16 = ieee_rint(g_16minus)

            if (res1_16 /= 3.0_16 ) error stop 23
            if (res2_16 /= -3.0_16 ) error stop 24
            if (gres1_16 /= 9.0_16 ) error stop 25
            if (gres2_16 /= -9.0_16 ) error stop 26

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 60
            enddo



! test +INF

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_up)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_up) error stop 27

            ! test real*4
            res1 = ieee_rint(pi)
            res2 = ieee_rint(pi_minus)
            gres1 = ieee_rint(g)
            gres2 = ieee_rint(g_minus)

            if (res1 /= 4.0 ) error stop 28
            if (res2 /= -3.0 ) error stop 29
            if (gres1 /= 10.0 ) error stop 30
            if (gres2 /= -9.0 ) error stop 31


            !  test real*8
            res1_8 = ieee_rint(pi_8)
            res2_8 = ieee_rint(pi_8minus)
            gres1_8 = ieee_rint(g_8)
            gres2_8 = ieee_rint(g_8minus)

            if (res1_8 /= 4.0_8 ) error stop 32
            if (res2_8 /= -3.0_8 ) error stop 33
            if (gres1_8 /= 10.0_8 ) error stop 34
            if (gres2_8 /= -9.0_8 ) error stop 35


            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 70
            enddo

            !  test real*16
            res1_16 = ieee_rint(pi_16)
            res2_16 = ieee_rint(pi_16minus)
            gres1_16 = ieee_rint(g_16)
            gres2_16 = ieee_rint(g_16minus)

            if (res1_16 /= 4.0_16 ) error stop 36
            if (res2_16 /= -3.0_16 ) error stop 37
            if (gres1_16 /= 10.0_16 ) error stop 38
            if (gres2_16 /= -9.0_16 ) error stop 39

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 70
            enddo



! test to -INF

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_down)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_down) error stop 40

            ! test real*4
            res1 = ieee_rint(pi)
            res2 = ieee_rint(pi_minus)
            gres1 = ieee_rint(g)
            gres2 = ieee_rint(g_minus)

            if (res1 /= 3.0 ) error stop 41
            if (res2 /= -4.0 ) error stop 42
            if (gres1 /= 9.0 ) error stop 43
            if (gres2 /= -10.0 ) error stop 44


            !  test real*8
            res1_8 = ieee_rint(pi_8)
            res2_8 = ieee_rint(pi_8minus)
            gres1_8 = ieee_rint(g_8)
            gres2_8 = ieee_rint(g_8minus)

            if (res1_8 /= 3.0_8 ) error stop 45
            if (res2_8 /= -4.0_8 ) error stop 46
            if (gres1_8 /= 9.0_8 ) error stop 47
            if (gres2_8 /= -10.0_8 ) error stop 48

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 80
            enddo

            !  test real*16
            res1_16 = ieee_rint(pi_16)
            res2_16 = ieee_rint(pi_16minus)
            gres1_16 = ieee_rint(g_16)
            gres2_16 = ieee_rint(g_16minus)

            if (res1_16 /= 3.0_16 ) error stop 49
            if (res2_16 /= -4.0_16 ) error stop 50
            if (gres1_16 /= 9.0_16 ) error stop 51
            if (gres2_16 /= -10.0_16 ) error stop 52

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,4
                if (flag_values(i) .neqv. .false.)error stop 80
            enddo


         end program
