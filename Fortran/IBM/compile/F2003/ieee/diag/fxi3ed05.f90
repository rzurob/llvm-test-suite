! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_GET_RONDING_MODE
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
        program fxi3ed05

        use ieee_arithmetic

        integer :: i
        real :: x
		type(ieee_round_type) :: r_type
		type(ieee_status_type) :: s_type

        call ieee_get_rounding_mode()
        call ieee_get_rounding_mode(i)
        call ieee_get_rounding_mode(x)
        call ieee_get_rounding_mode(r_type, x)
		call ieee_get_rounding_mode(s_type)

        end
