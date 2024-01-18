! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB with complex variables.
!*  SECONDARY FUNCTIONS TESTED : -qautodbl=dblpad4
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Only real*4 should be promoted .
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program logb_variab

        use ieee_arithmetic

        real*4 :: y4, x4
        real*8 :: y8, x8
        real*16 :: y16, x16
	integer :: i, P, R, Z
	logical :: flag_values(5)


!       Test real*4

        call ieee_set_flag(ieee_all,.false.)


        x4 = 3.0

        P = precision(x4)
        R = range(x4)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 8) error stop 100


        y4 = ieee_logb(x4)

        if (y4 /= exponent(x4)-1) error stop 1
					!"ieee_logb failed for real*4."

        P = precision(y4)
        R = range(y4)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 8) error stop 102


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(10+i)
        enddo



!       Test real*8

        call ieee_set_flag(ieee_all,.false.)

        x8 = -65536.0D5

        P = precision(x8)
        R = range(x8)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 8) error stop 103


        y8 = ieee_logb(x8)

        if (y8 /= exponent(x8)-1) error stop 2
                                        !"ieee_logb failed for real*4."

        P = precision(y8)
        R = range(x8)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 8) error stop 104


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(20+i)
        enddo




!       test real*16

        call ieee_set_flag(ieee_all,.false.)


        x16 = 98732.6Q2


        P = precision(x16)
        R = range(x16)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 105

        y16 = ieee_logb(x16)
        if (y16 /= exponent(x16)-1) error stop 3
                                        !"ieee_logb failed for real*4."

        P = precision(y16)
        R = range(y16)
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 106


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(30+i)
        enddo


	end program

