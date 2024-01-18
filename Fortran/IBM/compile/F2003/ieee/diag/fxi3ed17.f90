! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : data type not supported
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxi3ed17

        use ieee_exceptions

        integer :: i
		real :: x
        logical :: yn
		type(ieee_flag_type) :: flag
		type(ieee_status_type) :: sta_typ

        call ieee_set_halting_mode()
		call ieee_set_halting_mode(i)
		call ieee_set_halting_mode(x)
		call ieee_set_halting_mode(yn)
		call ieee_set_halting_mode(flag)
		call ieee_set_halting_mode(x, yn)
		call ieee_set_halting_mode(i, yn)
		call ieee_set_halting_mode(x, i)
        call ieee_set_halting_mode(x, x)
		call ieee_set_halting_mode(flag, yn, i)
		call ieee_set_halting_mode(sta_type, yn)

		end
