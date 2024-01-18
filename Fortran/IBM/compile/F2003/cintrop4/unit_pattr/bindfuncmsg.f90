!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Variables with BINDC attribute only appears
!*                               in the specification part of a module
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : BIND(C)
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : make sure correct error message is thrown
!*                               when bind(c) is improperly used
!*
!234567890123456789012345678901234567890123456789012345678901234567890
!
! Tests bind(c) error message on function variables
!
  FUNCTION fun1() bind(c)
     character, bind(c) :: fun1
     fun1= 'c'
  END FUNCTION fun1
