! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB
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
        program fxi3ed14

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        y = ieee_scalb(x,y)
		y = ieee_scalb()
		y = ieee_scalb(x)
		y = ieee_scalb(i)
		y = ieee_scalb(x, i, y)

        end
