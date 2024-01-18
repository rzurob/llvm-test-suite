! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_ROUNDING
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
        program fxi3ed28

        use ieee_exceptions
		use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		type(ieee_flag_type) :: f_type
		logical :: yn

        yn = ieee_support_rounding()
		yn = ieee_support_rounding(i)
        yn = ieee_support_rounding(x, i)
		yn = ieee_support_rounding(r_type, i)
		yn = ieee_support_rounding(x)
		yn = ieee_support_rounding(i,f_type)
		yn = ieee_support_rounding(f_type, x)

        end
