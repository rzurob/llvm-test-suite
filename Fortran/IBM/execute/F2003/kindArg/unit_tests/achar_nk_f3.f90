!*  ===================================================================
!*
!*  TEST CASE NAME             : achar_nk_f3.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test achar functionality w/o kind specified
!*                               , -qintsize=4, default kind should override
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1
           character*1 :: r1

	   k1 = kind(achar(88))
           if (k1 .ne. 1) error stop 1

           r1 = achar(88)
	   if (r1 .ne. 'X') error stop 2

END
