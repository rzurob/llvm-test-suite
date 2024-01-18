! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with complex numbers.
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

 program rint_complex


         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         integer :: i
         complex(4) :: cx
         real*4 :: res1, res2
         complex(8) :: cx_8
         real*8 :: res1_8, res2_8
         complex(16) :: cx_16
         real*16 :: res1_16, res2_16
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
         if (rtype /= rt_nearest)  error stop 1
         call ieee_get_status(status_value)

             !  test real*4
             cx = (2345.89, -34367.43)
             res1 = ieee_rint(real(cx))
             res2 = ieee_rint(imag(cx))
             if (res1 /= 2346.0 ) error stop 2
             if (res2 /= -34367.0 ) error stop 3

             !  test real*8
             cx_8 = (2345.89_8,-34367.3_8)
             res1_8 = ieee_rint(real(cx_8))
             res2_8 = ieee_rint(imag(cx_8))
             if (res1_8 /= 2346.0 ) error stop 4
             if (res2_8 /= -34367.0 ) error stop 5

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 100
             enddo

             !  test real*16
             cx_16 = (2345.89_16,-34367.3_16)
             res1_16 = ieee_rint(real(cx_16))
             res2_16 = ieee_rint(imag(cx_16))
             if (res1_16 /= 2346.0 ) error stop 7
             if (res2_16 /= -34367.0 ) error stop 8

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 100
             enddo


! test  to zero

            call ieee_set_status(status_value)
            call ieee_set_rounding_mode(rt_20)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_20) error stop 9
            call ieee_get_status(status_value)

            ! test real*4
            cx = (2345.89, -34367.9)
            res1 = ieee_rint(real(cx))
            res2 = ieee_rint(imag(cx))
            if (res1 /= 2345.0 ) error stop 10
            if (res2 /= -34367.0 ) error stop 11

            !  test real*8
            cx_8 = (2345.89_8, -34367.63_8)
            res1_8 = ieee_rint(real(cx_8))
            res2_8 = ieee_rint(imag(cx_8))
            if (res1_8 /= 2345.0 ) error stop 12
            if (res2_8 /= -34367.0 ) error stop 13

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo

            !  test real*16
            cx_16 = (2345.89_16, -34367.93_16)
            res1_16 = ieee_rint(real(cx_16))
            res2_16 = ieee_rint(imag(cx_16))
            if (res1_16 /= 2345.0 ) error stop 14
            if (res2_16 /= -34367.0 ) error stop 15

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo


! test +INF

            call ieee_set_status(status_value)
            call ieee_set_rounding_mode(rt_up)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_up) error stop 16
            call ieee_get_status(status_value)

             ! test real*4
             cx = (2345.12, -34367.9)
             res1 = ieee_rint(real(cx))
             res2 = ieee_rint(imag(cx))
             if (res1 /= 2346.0 ) error stop 17
             if (res2 /= -34367.0 ) error stop 18

             !  test real*8
             cx_8 = (2345.29_8,-34367.63_8)
             res1_8 = ieee_rint(real(cx_8))
             res2_8 = ieee_rint(imag(cx_8))
             if (res1_8 /= 2346.0 ) error stop 19
             if (res2_8 /= -34367.0 ) error stop 20

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo

             !  test real*16
             cx_16 = (2345.19_16, -34367.93_16)
             res1_16 = ieee_rint(real(cx_16))
             res2_16 = ieee_rint(imag(cx_16))
             if (res1_16 /= 2346.0 ) error stop 21
             if (res2_16 /= -34367.0 ) error stop 22

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo


! test to -INF

             call ieee_set_status(status_value)
             call ieee_set_rounding_mode(rt_down)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_down) error stop 22
             call ieee_get_status(status_value)

             ! test real*4
             cx = (2345.912, -34367.29)
             res1 = ieee_rint(real(cx))
             res2 = ieee_rint(imag(cx))
             if (res1 /= 2345.0 ) error stop 23
             if (res2 /= -34368.0 ) error stop 24

             !  test real*8
             cx_8 = (2345.829_8, -34367.163_8)
             res1_8 = ieee_rint(real(cx_8))
             res2_8 = ieee_rint(imag(cx_8))
             if (res1_8 /= 2345.0 ) error stop 25
             if (res2_8 /= -34368.0 ) error stop 26

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 400
             enddo

             !  test real*16
             cx_16 = (2345.819_16, -34367.293_16)
             res1_16 = ieee_rint(real(cx_16))
             res2_16 = ieee_rint(imag(cx_16))
             if (res1_16 /= 2345.0 ) error stop 27
             if (res2_16 /= -34368.0 ) error stop 28

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 400
             enddo

end
