! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee40
! %COMPOPTS: -qflttrap -qfree=f90
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
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qflttrap
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test set_halting_mode to false for real*8
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee40

         use ieee_exceptions

  		 real*8 :: xr_8, yr_8, zr_8, r8, tmp_8
	     type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*8

         xr_8 = 3.0_8
	     yr_8 = 0.0_8
	     call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .false.)
	     !print *, "The program will not halt on IEEE_DIVIDE_BY_ZERO!"
	     zr_8 = xr_8 / yr_8
		 tmp_8 = zr_8

	     xr_8 = 0.0_8
	     call ieee_set_halting_mode(IEEE_INVALID, .false.)
	     !print *, "The program will not halt on IEEE_INVALID!"
	     zr_8 = xr_8 / yr_8
		 tmp_8 = zr_8

         xr_8 = huge(1.0_8)
	     call ieee_set_halting_mode(IEEE_OVERFLOW, .false.)
	    ! print *, "The program will not halt on IEEE_OVERFLOW!"
         zr_8 = 2.0_8 * xr_8
		 tmp_8 = zr_8

		 xr_8 = tiny(PINF_8)
	     call ieee_set_halting_mode(IEEE_UNDERFLOW, .false.)
	    ! print *, "The program will not halt on IEEE_OVERFLOW!"
        zr_8 = 0.5_8 * xr_8
		tmp_8 = zr_8

        xr_8 = z"0000000000000001"
		yr_8 = 2.0
		call ieee_set_halting_mode(IEEE_INEXACT, .false.)
		 ! print *, "The program will not halt on IEEE_INEXACT!"
        zr_8 = xr_8 / yr_8
		tmp_8 = zr_8

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
