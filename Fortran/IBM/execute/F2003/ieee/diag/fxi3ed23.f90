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
! %POSTCMD: dcomp fxi3ed23.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_FLAG
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
        program fxi3ed23

        use ieee_exceptions
		use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		type(ieee_flag_type) :: f_type
		logical :: yn

        yn = ieee_support_flag()
		yn = ieee_support_flag(i)
        yn = ieee_support_flag(x, i)
		yn = ieee_support_flag(r_type, i)
		yn = ieee_support_flag(x)
		yn = ieee_support_flag(i,f_type)
		yn = ieee_support_flag(f_type, i)

        end
