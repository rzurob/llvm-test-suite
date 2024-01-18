! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee39
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
!*  DESCRIPTION                : Test set_halting_mode to false for real*4
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee39

         use ieee_exceptions

         real :: xr, yr, zr, tmp
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*4

         xr = 3.0
	     yr = 0.0
	     call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .false.)
	    ! print *, "The program will not halt on IEEE_DIVIDE_BY_ZERO!"
	     zr = xr / yr
		 tmp = zr

		 xr = 0.0
	     call ieee_set_halting_mode(IEEE_INVALID, .false.)
	     !print *, "The program will not halt on IEEE_INVALID!"
	     zr = xr / yr
		 tmp = zr

		 xr = huge(1.0)
	     call ieee_set_halting_mode(IEEE_OVERFLOW, .false.)
	     !print *, "The program will not halt on IEEE_OVERFLOW!"
         zr = 2.0 * xr
		 tmp = zr

	     xr = tiny(1.0)
		 call ieee_set_halting_mode(IEEE_UNDERFLOW, .false.)
	     !print *, "The program will not halt on IEEE_OVERFLOW!"
         zr = 0.5 * xr
		 tmp = zr

         xr = z"00000001"
		 yr = 2.0
         zr = xr / yr
         call ieee_set_halting_mode(IEEE_INEXACT, .false.)
		 !print *, "The program will not halt on IEEE_INEXACT!"
		 zr = xr / yr
         tmp = zr

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
