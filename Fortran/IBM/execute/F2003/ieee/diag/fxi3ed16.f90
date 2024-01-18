! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fxi3ed16.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_FLAG
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
        program fxi3ed16

        use ieee_exceptions

        integer :: i
		real :: x
        logical :: yn
		type(ieee_flag_type) :: flag
		type(ieee_status_type) :: sta_typ

        call ieee_set_flag()
		call ieee_set_flag(i)
		call ieee_set_flag(x)
		call ieee_set_flag(yn)
		call ieee_set_flag(flag)
		call ieee_set_flag(x, yn)
		call ieee_set_flag(i, yn)
		call ieee_set_flag(x, i)
        call ieee_set_flag(x, x)
		call ieee_set_flag(flag, yn, i)
		call ieee_set_flag(sta_type, yn)

		end
