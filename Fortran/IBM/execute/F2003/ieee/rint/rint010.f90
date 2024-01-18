! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with reals < 1.0 and >-1.0.
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

         program rint_real

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         real*4 :: xr1, res1, xr2, res2
         real*8 :: xr1_8, res1_8, xr2_8, res2_8
         real*16 :: xr1_16, res1_16, xr2_16, res2_16
         integer :: i, ires1, ires2, ires1_8, ires2_8, ires1_16, ires2_16
         logical :: flag_values(5)

         equivalence(res1,ires1)
         equivalence(res2,ires2)
         equivalence(res1_8,ires1_8)
         equivalence(res2_8,ires2_8)
         equivalence(res1_16,ires1_16)
         equivalence(res2_16,ires2_16)


         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
         type(ieee_round_type), parameter :: rt_up = IEEE_UP
         type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
         type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
         type(ieee_status_type) :: status_value


!  test nereast

         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_nearest)  error stop 1
         call ieee_get_status(status_value)

            !  test real*4
            xr1 = 0.0
            xr2 = -0.0
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000" .and. res1 /= 0.0) error stop 2
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 3

            !  test real*4
            xr1 = 0.89
            xr2 = -0.3
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 1.0) error stop 4
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 5

            !  test real*8
            xr1_8 = 0.0_8
            xr2_8 = -0.0_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 6
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 7

            !  test real*8
            xr1_8 = 0.89_8
            xr2_8 = -0.3_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 1.0 ) error stop 8
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 9

            ! Now check that no flags were turned on.
            call ieee_get_flag(ieee_all,flag_values)
            do i = 1,5
                if (flag_values(i) .neqv. .false.)error stop 100
            enddo

            !  test real*16
            xr1_16 = 0.0_16
            xr2_16 = -0.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 10
            if (ires2_16 /= z"00000000".and. res2_16 /= 0.0) error stop 11


            !  test real*16
            xr1_16 = 0.89_16
            xr2_16 = -0.3_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 1.0 ) error stop 12
            if (res2_16 /= 0.0) error stop 13

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 100
             enddo


! test  to zero

           call ieee_set_status(status_value)
           call ieee_set_rounding_mode(rt_20)
           call ieee_get_rounding_mode(rtype)
           if (rtype /= rt_20) error stop 14
           call ieee_get_status(status_value)

            !  test real*4
            xr1 = 0.0
            xr2 = -0.0
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000".and. res1 /= 0.0) error stop 15
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 16

            ! test real*4
            xr1 = 0.89
            xr2 = -0.9
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000".and. res1 /= 0.0) error stop 17
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 18

            !  test real*8
            xr1_8 = 0.0_8
            xr2_8 = -0.0_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 19
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 20

            !  test real*8
            xr1_8 = 0.89_8
            xr2_8 = -0.63_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 21
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 22

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo

            !  test real*16
            xr1_16 = 0.0_16
            xr2_16 = -0.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 23
            if (ires2_16 /= z"00000000".and. res2_16 /= 0.0) error stop 24

            !  test real*16
            xr1_16 = 0.89_16
            xr2_16 = -0.93_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 25
            if (ires2_16 /= z"80000000".and. res2_16 /= 0.0) error stop 26


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo


! test +INF

           call ieee_set_status(status_value)
           call ieee_set_rounding_mode(rt_up)
           call ieee_get_rounding_mode(rtype)
           if (rtype /= rt_up) error stop 27
           call ieee_get_status(status_value)

            !  test real*4
            xr1 = 0.0
            xr2 = -0.0
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000".and. res1 /= 0.0) error stop 28
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 29

            ! test real*4
            xr1 = 0.12
            xr2 = -0.9
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 1.0) error stop 30
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 31

            !  test real*8
            xr1_8 = 0.0_8
            xr2_8 = -0.0_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 32
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 33

            !  test real*8
            xr1_8 = 0.29_8
            xr2_8 = -0.63_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 1.0) error stop 34
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 35


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo

            !  test real*16
            xr1_16 = 0.0_16
            xr2_16 = -0.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 36
            if (ires2_16 /= z"00000000".and. res2_16 /= 0.0) error stop 37

            !  test real*16
            xr1_16 = 0.19_16
            xr2_16 = -0.93_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 1.0) error stop 38
            if (ires2_16 /= z"80000000".and. res2_16 /= 0.0) error stop 39


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo


! test to -INF

           call ieee_set_status(status_value)
           call ieee_set_rounding_mode(rt_down)
           call ieee_get_rounding_mode(rtype)
           if (rtype /= rt_down) error stop 40
           call ieee_get_status(status_value)

            !  test real*4
            xr1 = 0.0
            xr2 = -0.0
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000".and. res1 /= 0.0) error stop 41
            if (ires2 /= z"80000000".and. res2 /= 0.0) error stop 42

            ! test real*4
            xr1 = 0.912
            xr2 = -0.29
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= z"00000000".and. res1 /= 0.0) error stop 43
            if (res2 /= -1.0) error stop 44

            !  test real*8
            xr1_8 = 0.0_8
            xr2_8 = -0.0_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 45
            if (ires2_8 /= z"80000000".and. res2_8 /= 0.0) error stop 46

            !  test real*8
            xr1_8 = 0.829_8
            xr2_8 = -0.163_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= z"00000000".and. res1_8 /= 0.0) error stop 47
            if (res2_8 /= -1.0) error stop 48

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 400
             enddo

            !  test real*16
            xr1_16 = 0.0_16
            xr2_16 = -0.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 49
            if (ires2_16 /= z"00000000".and. res2_16 /= 0.0) error stop 50

            !  test real*16
            xr1_16 = 0.819_16
            xr2_16 = -0.293_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16 /= z"00000000".and. res1_16 /= 0.0) error stop 51
            if (res2_16 /= -1.0) error stop 52


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 400
             enddo

         end program
