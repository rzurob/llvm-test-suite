!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ichar_nk_f1.f
!*
!*  PROGRAMMER                 : Vince Yuen
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test ichar functionality without kind
!*                               specified, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890


           integer :: k1, r1, r2, r3
           character (32) c1, c2, c3
           c1 = achar(100)
           c2 = achar(23)
           c3 = achar(87)

           k1 = kind(ichar(c1));
           if (k1 .ne. 4) error stop 1

           r1 = ichar(c1);
	   if (r1 .ne. 100) error stop 2

           r2 = ichar(c2);
	   if (r2 .ne. 23) error stop 3

           r3 = ichar(c3);
	   if (r3 .ne. 87) error stop 4

END
