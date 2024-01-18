! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_COPY_SIGN
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
        program fxi3ed02

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        x = 8.0
        i = 1
        y = ieee_copy_sign(x,i)
		y = ieee_copy_sign()
		y = ieee_copy_sign(x)
		y = ieee_copy_sign(i,i)

        end
