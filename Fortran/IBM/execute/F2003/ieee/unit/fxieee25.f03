! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_HALTING_MODE
!*                               IEEE_SUPPORT_HALTING_MODE
!*
!*  REQUIRED COMPILER OPTIONS  : -qflttrap
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : test program will halting on IEEE_INVALID
!*                               for real*4
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee25

         use ieee_exceptions
         use constants_for_ieee

         real :: xr, yr, zr, tmp
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*4

         yr = 0.0
	     xr = 0.0

	     call ieee_set_halting_mode(IEEE_INVALID, .true.)
		 ! print *, "The program will halt on IEEE_INVALID!"
	     zr = xr / yr
		 tmp = zr

! restore the original falgs.
        call ieee_set_status(status_value)

        end program