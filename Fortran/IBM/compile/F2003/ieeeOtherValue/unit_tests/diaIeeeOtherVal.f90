!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 4,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : Add IEEE_OTHER_VALUE
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
!*  Diagnostic test for the feature 338353. Added IEEE_OTHER_VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    USE,INTRINSIC :: IEEE_ARITHMETIC
    type(ieee_class_type) :: k = ieee_other_value
    type(ieee_class_type) :: r = ieee_other_value_wrong
end
