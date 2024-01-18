!*  ===================================================================
!*
!*  TEST CASE NAME             : achar_k8_d1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : diagnostic test with invalid kind of 8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1

	   k1 = kind(achar(88, 8))

END
