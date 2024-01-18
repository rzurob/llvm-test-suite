! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh scalb002
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
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with complex numbers.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase tests that the real
!* parts of complex numbers can be used as arguments for this function.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program scalb_complex
        use ieee_arithmetic

	implicit none
        real*4 ::  result4, result_4
	real*8 ::  result8, result_8
        real*16 :: result16, result_16
        complex(4) :: cx
        complex(8) :: cx_8
        complex(16) :: cx_16
        integer :: yi, i

        logical :: flag_values(5)
        logical :: flag_value
        type(ieee_status_type) :: status_value

! Test positive real(4) / imaginary(4) of complex for yi = 0

        !*  Get the current value of the floating point status
        call ieee_get_status(status_value)

        cx = (2345.89, 34367.43)
        yi = 0
        result4 = ieee_scalb(real(cx), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 101
        enddo

	result_4 = 2.0_4**yi*real(cx)
        if (result4 /= result_4) error stop 1

        !*  Restore the floating point status.
        call ieee_set_status(status_value)

        result4 = ieee_scalb(imag(cx), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 102
        enddo

        result_4 = 2.0_4**yi*imag(cx)
        if (result4 /= result_4) error stop 2


! Test negative real(4) / imaginary(4) of complex for yi = 0

        !*  Restore the floating point status.
        call ieee_set_status(status_value)

        cx = (-2345.89, -34367.43)
        yi = 0
        result4 = ieee_scalb(real(cx), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 103
        enddo

        result_4 = 2.0_4**yi*real(cx)
        if (result4 /= result_4) error stop 3

        !*  Restore the floating point status.
        call ieee_set_status(status_value)

        result4 = ieee_scalb(imag(cx), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 104
        enddo

        result_4 = 2.0_4**yi*imag(cx)
        if (result4 /= result_4) error stop 4


! Test positive real(8) / imaginary(8) of complex for yi = 1

        call ieee_set_status(status_value)

        cx_8 = (2345.89_8, 67.43_8)
        yi = 1
        result8 = ieee_scalb(real(cx_8), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 105
        enddo

        result_8 = 2.0_8**yi*real(cx_8)
        if (result8 /= result_8) error stop 5

        call ieee_set_status(status_value)

        result8 = ieee_scalb(imag(cx_8), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 106
        enddo

        result_8 = 2.0_8**yi*imag(cx_8)
        if (result8 /= result_8) error stop 6


! Test negative real(8) / imaginary(8) of complex for yi = 1

        call ieee_set_status(status_value)

        cx_8 = (-2345.89, -34367.89)
        yi = 1
        result8 = ieee_scalb(real(cx_8), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 107
        enddo

        result_8 = 2.0_8**yi*real(cx_8)
        if (result8 /= result_8) error stop 7

        call ieee_set_status(status_value)

        result8 = ieee_scalb(imag(cx_8), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 108
        enddo

        result_8 = 2.0_8**yi*imag(cx_8)
        if (result8 /= result_8) error stop 8


! Test positive real(16) / imaginary(16) of complex for yi = 1

        call ieee_set_status(status_value)

        cx_16 = (25.0_16, 7.0_16)
        yi = 1
        result16 = ieee_scalb(real(cx_16), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 109
        enddo

        result_16 = 2.0_16**yi*real(cx_16)
        if (result16 /= result_16) error stop 9

        call ieee_set_status(status_value)

        result16 = ieee_scalb(imag(cx_16), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 110
        enddo

        result_16 = 2.0_16**yi*imag(cx_16)
        if (result16 /= result_16) error stop 10


! Test negative real(16) / imaginary(16) of complex for yi = 2

        call ieee_set_status(status_value)

        cx_16 = (-25.0_16, -7.0_16)
        yi = 2
        result16 = ieee_scalb(real(cx_16), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 111
        enddo

        result_16 = 2.**yi*real(cx_16)
        if (result16 /= result_16) error stop 11

        call ieee_set_status(status_value)

        result16 = ieee_scalb(imag(cx_16), yi)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 112
        enddo

        result_16 = 2.0_16**yi*imag(cx_16)
        if (result16 /= result_16) error stop 12

        end program

