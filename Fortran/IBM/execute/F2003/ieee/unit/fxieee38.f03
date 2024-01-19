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
!*  DESCRIPTION                : test program will halting on IEEE_INEXACT
!*                               for real*16
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee38

         use ieee_exceptions
         use constants_for_ieee

         real*16 :: xr16, yr16, zr16
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*16

         xr16 = z"0700000000000000000000000000001"
		 yr16 = 2.0
		 ! zr16 = xr16 / yr16
	     call ieee_set_halting_mode(IEEE_INEXACT, .true.)
		 ! print *, "The program will halt on IEEE_INEXACT!"
         zr16 = xr16 / yr16

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
