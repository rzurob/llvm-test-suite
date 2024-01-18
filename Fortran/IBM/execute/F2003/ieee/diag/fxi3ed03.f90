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
! %POSTCMD: dcomp fxi3ed03.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_GET_FLAG
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
        program fxi3ed03

        use ieee_exceptions

        integer :: i
		real :: x
        logical :: yn
		type(ieee_flag_type) :: flag

        call ieee_get_flag()
		call ieee_get_flag(i)
		call ieee_get_flag(x)
		call ieee_get_flag(yn)
		call ieee_get_flag(flag)
		call ieee_get_flag(x, yn)
		call ieee_get_flag(i, yn)
		call ieee_get_flag(x, i)
        call ieee_get_flag(x, x)
		call ieee_get_flag(flag, yn, i)
        end
