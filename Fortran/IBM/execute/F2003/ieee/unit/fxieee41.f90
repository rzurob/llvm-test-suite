! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee41
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
!*  DESCRIPTION                : Test set_halting_mode to false real*16
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee41

         use ieee_exceptions
		 real*16 :: xr_16, yr_16, zr_16, r16, tmp_16
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*16

        xr_16 = 3.0_16
        yr_16 = 0.0_16
	     call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .false.)
	    ! print *, "The program will not halt on IEEE_DIVIDE_BY_ZERO!"
	     zr_16 = xr_16 / yr_16

         xr_16 = 0.0_16
	     call ieee_set_halting_mode(IEEE_INVALID, .false.)
	     !print *, "The program will not halt on IEEE_INVALID!"
	     zr_16 = xr_16 / yr_16

	     xr_16 = huge(1.0_16)
	     call ieee_set_halting_mode(IEEE_OVERFLOW, .false.)
	     !print *, "The program will not halt on IEEE_OVERFLOW!"
         zr_16 = 2.0_16 * xr_16

         xr_16 = tiny(1.0_16)
	     call ieee_set_halting_mode(IEEE_UNDERFLOW, .false.)
	     !print *, "The program will not halt on IEEE_OVERFLOW!"
         zr_16 = xr_16 * xr_16

         xr_16 = z"0700000000000000000000000000001"
		 yr_16 = 2.0
         call ieee_set_halting_mode(IEEE_INEXACT, .false.)
		 ! print *, "The program will not halt on IEEE_INEXACT!"
         zr_16 = xr_16 / yr_16

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
