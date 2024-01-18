! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_VALUE
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
        program fxi3ed32

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y
		logical :: yn

        yn = ieee_value(x,i)
		yn = ieee_value()
		yn = ieee_value(x, y)
		yn = ieee_value(result)
		yn = ieee_value(i,i)
		yn = ieee_value(i,y)
		yn = ieee_value(x, result, i)

        end
