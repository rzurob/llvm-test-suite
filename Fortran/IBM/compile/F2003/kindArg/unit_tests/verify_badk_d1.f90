!*  ===================================================================
!*
!*  TEST CASE NAME             : verify_badk_d1.f
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

           integer k1, r1, r2

	   r1 = verify('APJG', 'A');
           if (r1 .ne. 2) error stop 1

	   k1 = kind(verify('APPLE', 'A', BACK = .TRUE., Kind = r1S));

	   r2 = verify('IEHDJS', 'D', KIND = 989);
END
