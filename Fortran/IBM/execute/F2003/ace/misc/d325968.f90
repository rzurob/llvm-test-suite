!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ieee_value using real part of complex variable
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ieee, complex, real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the real portion of a complex variable can be used in calculating
!*  an IEEE constant and assigning it to that same variable.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d325968

  use, intrinsic :: ieee_arithmetic
  implicit none

  complex :: fnan

  fnan = 0.0
  fnan = ieee_value(real(fnan), IEEE_QUIET_NAN)
  if( .not. ieee_is_nan(real(fnan))) stop 2

  fnan = ieee_value(real(fnan), IEEE_POSITIVE_INF)
  if( ieee_class(real(fnan)) /= IEEE_POSITIVE_INF) stop 3

  fnan = ieee_value(real(fnan), IEEE_NEGATIVE_INF)
  if( ieee_class(real(fnan)) /= IEEE_NEGATIVE_INF) stop 4

end program d325968
