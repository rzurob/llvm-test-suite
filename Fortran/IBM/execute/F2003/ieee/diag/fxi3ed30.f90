! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_STANDARD
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
        program fxi3ed30

        use ieee_arithmetic

        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		logical :: yn

        yn = ieee_support_standard(i)
        yn = ieee_support_standard(r_type, i)
		yn = ieee_support_standard(x, i)

        end
