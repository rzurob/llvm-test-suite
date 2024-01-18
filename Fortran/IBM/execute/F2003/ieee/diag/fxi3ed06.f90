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
! %POSTCMD: dcomp fxi3ed06.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_FINITE
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
        program fxi3ed06

        use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		logical :: yn

        yn = ieee_is_finite()
        yn = ieee_is_finite(i)
        yn = ieee_is_finite(r_type, i)
		yn = ieee_is_finite(x, i)

        end
