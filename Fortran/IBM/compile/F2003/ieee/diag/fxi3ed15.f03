! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SELECTED_REAL_KIND
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
        program fxi3ed15

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i, k
        real :: x, y

        k = ieee_selected_real_kind(x,i)
		k = ieee_selected_real_kind()
		k = ieee_selected_real_kind(x)
    	k = ieee_selected_real_kind(x, y, i)

        end
