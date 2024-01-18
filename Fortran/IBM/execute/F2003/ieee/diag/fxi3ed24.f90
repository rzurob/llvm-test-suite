! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_HALTING
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
        program fxi3ed24

        use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		type(ieee_flag_type) :: f_type
		logical :: yn

        yn = ieee_support_halting()
		yn = ieee_support_halting(i)
        yn = ieee_support_halting(r_type)
		yn = ieee_support_halting(x, i)
		yn = ieee_support_halting(x)
		yn = ieee_support_halting(f_type, x)

        end
