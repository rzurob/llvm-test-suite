! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh logb004
! %COMPOPTS: -qfree=f90
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB with complex variables.
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program logb_variab

        use ieee_arithmetic

        complex(4) :: x_4
        real*4 :: y_4
        complex(8) :: x_8
        real*8 :: y_8
        complex(16) :: x_16
        real*16 :: y_16
	integer :: i
	logical :: flag_values(5)
		

!       Test real*4

        call ieee_set_flag(ieee_all,.false.)


        x_4 = (3.0, -3.9)
        y_4 = ieee_logb(real(x_4))
        if (y_4 /= exponent(real(x_4))-1) error stop 1
					!"ieee_logb failed for real*4."

        y_4 = ieee_logb(imag(x_4))
        if (y_4 /= exponent(imag(x_4))-1) error stop 2
                                        !"ieee_logb failed for real*4."

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(20+i)
        enddo



!       Test real*8

        call ieee_set_flag(ieee_all,.false.)


        x_8 = (-65536.0D5, 32.6D2)

        y_8 = ieee_logb(real(x_8))
        if (y_8 /= exponent(real(x_8))-1) error stop 3
                                        !"ieee_logb failed for real*8."

        y_8 = ieee_logb(imag(x_8))
        if (y_8 /= exponent(imag(x_8))-1) error stop 4
                                        !"ieee_logb failed for real*8."

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(30+i)
        enddo


!       test real*16

        call ieee_set_flag(ieee_all,.false.)


        x_16 = (-636.0Q5, -98732.6Q2)

        y_16 = ieee_logb(real(x_16))
        if (y_16 /= exponent(real(x_16))-1) error stop 5
                                        !"ieee_logb failed for real*16."

        y_16 = ieee_logb(imag(x_16))
        if (y_16 /= exponent(imag(x_16))-1) error stop 6
                                        !"ieee_logb failed for real*16."

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(40+i)
        enddo



	end program

