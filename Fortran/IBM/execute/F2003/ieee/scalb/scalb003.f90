! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh scalb003
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with arrays.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IEEE_SCALB(x,i) = 2**i*x
!*  where the function arguments are real arrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

     program fxieee12
        use ieee_arithmetic
	implicit none

        real*4 :: xar4(2), result4(2)
        integer :: iar(2), i
	real*8 ::  xar8(2), result8(2)
        real*16 :: xar16(2), result16(2)

        logical :: flag_values(5)
        type(ieee_status_type) :: status_value

	call ieee_get_status(status_value)

!       Test real*4

	xar4 = (/16, 18/)
	iar = (/8, 19/)
	result4 = ieee_scalb(xar4, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 101
        enddo

	do i = 1, 2
	   xar4(i) = 2.0_4**iar(i)*xar4(i)
	   if ( result4(i) /= xar4(i) ) error stop 4
        enddo

!       Test real*8

        call ieee_set_status(status_value)

	xar8 = (/16_8, 18_8/)
	iar = (/12, 29/)
	result8 = ieee_scalb(xar8, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 201
        enddo

	do i = 1, 2
	   xar8(i) = 2.0_8**iar(i)*xar8(i)
	   if ( result8(i) /= xar8(i) ) error stop 8
        enddo

!    test real*16

        call ieee_set_status(status_value)

        xar16 = (/126.0_16, 168.0_16/)
	iar = (/9, 18/)
	result16 = ieee_scalb(xar16, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 301
        enddo

	do i = 1, 2
	   xar16(i) = (2.0_16**iar(i))*xar16(i)
	   if ( result16(i) /= xar16(i) ) error stop 16
        enddo

        end program

