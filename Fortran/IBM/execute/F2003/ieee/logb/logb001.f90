! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh logb001
! %COMPOPTS: -qfree=f90 -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : February 5, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB with real variables.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* 1.Test positive real*4/*8/*16
!* 2.Test negative real*4/*8/*16
!* 3.Test +/- ZERO  real*4/*8/*16
!* 4.Test +/- Denormals real*4/*8/*16
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program logb_variab

        use ieee_arithmetic
        use constants_for_ieee

	integer :: i
        real*4 :: x_4, y_4
        real*8 :: x_8, y_8
        real*16 :: x_16, y_16
        logical :: flag_values(5), flag_value, expect_value(5)
        type(ieee_status_type) :: status_value
		

!       Test real*4
 
        call ieee_set_flag(ieee_all,.false.)


        x_4 = 3.0
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1) error stop 1
					!"ieee_logb failed for real*4."
	x_4 = huge(1.0)
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 2
				        !"ieee_logb failed for real*4."
        x_4 = tiny(1.0)
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 3
                                        ! "ieee_logb failed for real*4."		

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 10
        enddo


!       Test real*8

        call ieee_set_flag(ieee_all,.false.)


        x_8 = 65536.0
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 4
                                          ! "ieee_logb failed for real*8."
        x_8 = huge(1.0_8)
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 5
                                          ! "ieee_logb failed for real*8."
        x_8 = tiny(1.0_8)
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 6 
                                          ! "ieee_logb failed for real*8."	


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        enddo


!       test real*16

        call ieee_set_flag(ieee_all,.false.)


        x_16 = 2.0_16**308
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 7
                                          ! "ieee_logb failed real*16."
        call ieee_set_flag(ieee_all, .false.)
        x_16 = huge(1.0_16)
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 8
                                          ! "ieee_logb failed for real*16."
        x_16 = tiny(1.0_16)
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 9
                                          ! "ieee_logb failed for real*16."


!         Now check that no flags were turned on.
!        call ieee_get_flag(ieee_all,flag_values)
!        do i = 1,5
!            if (flag_values(i) .neqv. .false.)error stop 30
!        enddo



!       Test real*4

        call ieee_set_flag(ieee_all,.false.)


        x_4 = -3.0
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 10
                                         !"ieee_logb failed real*4 w/ negative."

!       Test real*8

        x_8 = -65536.0
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 11 
                                        ! "ieee_logb failed real*8 w/ negative."

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        enddo


!       test real*16

        x_16 = -2.0_16**308
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 12 
                                       !"ieee_logb failed real*16 w/ negative.."

!        ! Now check that no flags were turned on.
!        call ieee_get_flag(ieee_all,flag_values)
!        do i = 1,5
!            if (flag_values(i) .neqv. .false.)error stop 10
!        enddo


!       Test zero 

        call ieee_set_flag(ieee_all,.false.)

        expect_value = (/.false.,.true.,.false.,.false.,.false./)

        x_4 = 0.0
        y_4 = ieee_logb(x_4)
        if (y_4 /= z"ff800000")error stop 13
                                         !"ieee_logb failed real*4 w/ 0

        call ieee_get_flag(ieee_all,flag_values)
	do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (30+i)
        end do

        call ieee_set_flag(ieee_all,.false.)
        x_4 = -0.0
        y_4 = ieee_logb(x_4)
        if (y_4 /= z"ff800000")error stop 14
                                         !"ieee_logb failed real*4 w/ 0

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (40+i)
        end do

        call ieee_set_flag(ieee_all,.false.)
        x_8 = 0.0_8
        y_8 = ieee_logb(x_8)
        if (y_8 /= z"fff0000000000000")error stop 15
                                       !"ieee_logb failed real*8 w/ 0

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (50+i)
        end do


        call ieee_set_flag(ieee_all,.false.)
        x_8 = -0.0_8
        y_8 = ieee_logb(x_8)
        if (y_8 /= z"fff0000000000000")error stop 16
                                       !"ieee_logb failed real*8 w/ 0

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (60+i)
        end do


        call ieee_set_flag(ieee_all,.false.)
        x_16 = 0.0_16
        y_16 = ieee_logb(x_16)
        if (y_16 /= z"fff00000000000000000000000000000")error stop 17
                                       !"ieee_logb failed real*16 w/ 0


        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (70+i)
        end do


!       Test Denormals real*4

        call ieee_set_flag(ieee_all,.false.)


        x_4 = PHD_4
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1) error stop 18
                                        !"ieee_logb failed for real*4."
        x_4 = PTD_4
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 19
                                        !"ieee_logb failed for real*4."
        x_4 = NHD_4
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 20
                                        ! "ieee_logb failed for real*4."

        x_4 = NTD_4
        y_4 = ieee_logb(x_4)
        if (y_4 /= exponent(x_4)-1)error stop 21
                                        ! "ieee_logb failed for real*4."



        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 80
        enddo


!       Test Denormals real*8

        call ieee_set_flag(ieee_all,.false.)


        x_8 = PHD_8
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 22
                                          ! "ieee_logb failed for real*8."
        x_8 = PTD_8
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 23
                                          ! "ieee_logb failed for real*8."
        
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 24
                                          ! "ieee_logb failed for real*8."

        x_8 = NTD_8
        y_8 = ieee_logb(x_8)
        if (y_8 /= exponent(x_8)-1)error stop 25
                                          ! "ieee_logb failed for real*8."



        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 90
        enddo


!       test Denormals real*16
!
!        call ieee_set_flag(ieee_all,.false.)

        x_16 = tiny(1.0_16)/2.0_16
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 26
                                          ! "ieee_logb failed for real*16.
        x_16 = -1.0_16*tiny(1.0_16)/2.0_16
        y_16 = ieee_logb(x_16)
        if (y_16 /= exponent(x_16)-1)error stop 27
                                          ! "ieee_logb failed for real*16.

!        ! Now check that no flags were turned on.
!        call ieee_get_flag(ieee_all,flag_values)
!        do i = 1,5
!            if (flag_values(i) .neqv. .false.)error stop 10
!        enddo

	end program

