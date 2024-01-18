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
!*  DESCRIPTION                : test program will halting on devide_by_zero
!*                               for real*8
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee29

         use ieee_exceptions
         use constants_for_ieee

         real*8 :: xr, yr, zr, tmp
		 type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

!  test real*8

         xr = 3.0_8
	     yr = 0.0_8

         call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .true.)
		! print *, "The program will halt on IEEE_DIVIDE_BY_ZERO!"
		 zr = xr / yr
		 tmp = zr

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
