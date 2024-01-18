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
! %POSTCMD: dcomp fxi3ed18.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_ROUNDING_MODE
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
        program fxi3ed18

        use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		type(ieee_status_type) :: sta_typ

        call  ieee_set_rounding_mode()
        call  ieee_set_rounding_mode(i)
		call  ieee_set_rounding_mode(x)
		call  ieee_set_rounding_mode(sta_typ)
        call  ieee_set_rounding_mode(r_type, i)
		call  ieee_set_rounding_mode(x, i)

        end
