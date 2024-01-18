! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB
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
        program fxi3ed10

        use ieee_arithmetic

        integer :: i
        real :: x, y
	    type(ieee_round_type) :: r_type

        y = ieee_logb()
        y = ieee_logb(i)
        y = ieee_logb(r_type)
		y = ieee_logb(x, i)

        end
