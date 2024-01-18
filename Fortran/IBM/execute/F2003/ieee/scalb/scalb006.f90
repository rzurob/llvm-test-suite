! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh scalb006
! %COMPOPTS: -qfree=f90 -qstrict
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
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with arguments as constants.
!*  SECONDARY FUNCTIONS TESTED :
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

        program scalb_const
        use ieee_arithmetic
	implicit none

	real*4 :: result4
	real*8 :: result8
	real*16 :: result16
        real*4, parameter :: pi=3.1415926, pi_minus=pi*(-1)
        integer :: yi, i
	real*8, parameter :: pi8 = 3.1415926_8,pi8_minus = pi8*(-1)
        real*16, parameter :: pi16 = 3.1415926,pi16_minus = pi16*(-1)

        logical :: flag_values(5)
        type(ieee_status_type) :: status_value

!       Test positive real*4 when yi = 0

        call ieee_get_status(status_value)
        yi = 0
        result4 = ieee_scalb(pi, yi)

        if (result4 /= pi ) error stop 1


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 10
        enddo

!       Test positive real*8 when yi = 0

        call ieee_set_status(status_value)
        yi = 0
        result8 = ieee_scalb(pi8, yi)

        if (result8 /= pi8 ) error stop 2

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        enddo

!       Test positive real*16 when yi = 0

        call ieee_set_status(status_value)
        yi = 0
        result16 = ieee_scalb(pi16, yi)

        if (result16 /= pi16 ) error stop 3

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        enddo

!       Test negative real*4 when yi = 0

        call ieee_set_status(status_value)
        yi = 0
        result4 = ieee_scalb(pi_minus, yi)

        if (result4 /= pi_minus ) error stop 4

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 40
        enddo

!       Test negative real*8 when yi = 0

        call ieee_set_status(status_value)
        yi = 0
        result8 = ieee_scalb(pi8_minus, yi)

        if (result8 /= pi8_minus ) error stop 5

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 50
        enddo

!       Test negative real*16 when yi = 0

        call ieee_set_status(status_value)
        yi = 0
        result16 = ieee_scalb(pi16_minus, yi)

        if (result16 /= pi16_minus ) error stop 6

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 60
        enddo

        end program

