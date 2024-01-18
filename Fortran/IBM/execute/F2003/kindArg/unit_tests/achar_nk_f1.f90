!*  ===================================================================
!*
!*  TEST CASE NAME             : achar_nk_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test achar functionality w/o kind specified
!*                               , no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1
           character*1 :: r1

           k1 = kind(achar(2))
           if (k1 .ne. 1) error stop 1

           r1 = achar(90)
	   if (r1 .ne. 'Z') error stop 2

END
