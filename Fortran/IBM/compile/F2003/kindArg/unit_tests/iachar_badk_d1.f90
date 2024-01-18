!*  ===================================================================
!*
!*  TEST CASE NAME             : iachar_badk_d1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test error with kind =an invalid value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1
           character (4) c1
           c1 = '8'

	   k1 = kind(iachar(c1, -823*383/23+3));

	   k2 = kind(iachar(c1, kind = Z'9'));

	   r1 = iachar(c1, k2);
END
