!*  ===================================================================
!*
!*  TEST CASE NAME             : achar_k4_d2.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : diagnostic test with invalid kind of 4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           character*1 :: r1

	   r1 = achar(88, 4)

END
