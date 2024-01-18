!*  ============================================================================
!*
!*  TEST CASE NAME             : intrinsic02D.f
!*
!*  DATE                       : 2010-11-10
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOS, ASIN, ATAN, COSH, SINH, TAN, TANH, ATAN, GAMMA,
!*                               ACOSH, ASINH, ATANH and LOG_GAMMA
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the extension to Trignometry intrinsic functions in F2008.
!*  Diagnostic test:
!*     Checking the level (77, 90, 95, 2003 and 2008) of Fortran language.
!*     Any level lower than 2008 should give warning message.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

   real result

   result = acos( (1.0, 0.0) )
   result = asin( (1.0, 0.0) )
   result = atan( (1.0, 0.0) )
   result = cosh( (1.0, 0.0) )
   result = sinh( (1.0, 0.0) )
   result = tan( (1.0, 0.0) )
   result = tanh( (1.0, 0.0) )
   result = atan(1.0, 1.0)            ! ATAN referring to ATAN2
   result = gamma(1.0)
   result = acosh(1.0)
   result = asinh(1.0)
   result = atanh(1.0)
   result = log_gamma(1.0)

END
