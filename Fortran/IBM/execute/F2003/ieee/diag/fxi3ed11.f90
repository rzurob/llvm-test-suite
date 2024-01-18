! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_NEXT_AFTER
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
        program fxi3ed11

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        y = ieee_next_after(x,i)
		y = ieee_next_after()
		y = ieee_next_after(x)
		y = ieee_next_after(i,i)
		y = ieee_next_after(x, y, i)

        end
