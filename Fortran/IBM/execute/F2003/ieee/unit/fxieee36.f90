! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee36
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
!*  DESCRIPTION                : test program will halting on IEEE_OVERFLOW
!*                               for real*16
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee36

         use ieee_exceptions
         use constants_for_ieee

         real*16 :: xr, zr, tmp
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*16

	     xr = huge(1.0_16)
	     call ieee_set_halting_mode(IEEE_OVERFLOW, .true.)
	     !print *, "The program will halt on IEEE_OVERFLOW!"
         zr = 2.0_16 * xr

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
