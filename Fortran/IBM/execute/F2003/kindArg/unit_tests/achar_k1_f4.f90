!*  ===================================================================
!*
!*  TEST CASE NAME             : achar_k1_f4.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test achar functionality w/ kind=1
!*                               (with and without keyword)
!*                               -qintsize = 8, default kind should override
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2
           character*1 :: r1, r2

	   k1 = kind(achar(86, 1))
           if (k1 .ne. 1) error stop 1

	   k2 = kind(achar(86, kind = 1))
           if (k2 .ne. 1) error stop 1

           r1 = achar(86, 1);
	   if (r1 .ne. 'V') error stop 3

           r2 = achar(86, kind =1);
	   if (r2 .ne. 'V') error stop 3

END
