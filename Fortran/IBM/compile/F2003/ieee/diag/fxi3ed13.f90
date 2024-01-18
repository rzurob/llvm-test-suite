! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT
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
        program fxi3ed13

        use ieee_arithmetic

        integer :: i
        real :: x, y
	    type(ieee_round_type) :: r_type

        y = ieee_rint()
        y = ieee_rint(i)
        y = ieee_rint(r_type)
		y = ieee_rint(x, i)

        end
