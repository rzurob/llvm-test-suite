!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 4,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_OTHER_VALUE
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 338353
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test IEEE_OTHER_VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    USE,INTRINSIC :: IEEE_ARITHMETIC
    type(ieee_class_type) :: k = ieee_other_value
    if (k /= ieee_other_value) then
      error stop 2_4
    endif
end
