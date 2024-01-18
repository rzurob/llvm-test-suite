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
! %POSTCMD: dcomp fxi3ed19.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_STATUS
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
        program fxi3ed19

        use ieee_exceptions

        integer :: i
        real :: x
	    type(ieee_flag_type) :: flag
		type(ieee_status_type) :: sta_typ

        call  ieee_set_status()
        call  ieee_set_status(i)
		call  ieee_set_status(x)
		call  ieee_set_status(sta_typ, i)
        call  ieee_set_status(falg)
		call  ieee_set_status(x, i)

        end
