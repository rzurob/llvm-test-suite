!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : stop_diag1.f
!*  TEST CASE TITLE            : Diagonostic test for stop statement 
!*
!*  PROGRAMMER                 : Tarique Islam
!*  DATE                       : Sept 29, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Unit test for STOP statement 
!*                             :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program p

  character(5),parameter  :: s = "tests" 
  integer, parameter :: r = 20
  character(1)    :: cv="a"
  real :: stop(10) 
  integer :: iv
  integer :: l
  stop
  stop 10000
  stop s
  stop "direct"
  stop 1_"kind test"
  ! Following should be flagged when langlvl is 
  ! F2003 or older
  stop 30 + 20
  stop "s1" // "test"
  stop "substring test"(4:10)
  stop r - 2
  stop r
  ! constant integer expression with kind
  stop 100_4
  ! Non-constant expression
  stop cv
  stop iv
  ! Verify if array "stop" and the "stop statement"
  ! are parsed correctly
  stop (10 + 20) * 40 - 12 
  stop = 40.0 + iv
end program p
