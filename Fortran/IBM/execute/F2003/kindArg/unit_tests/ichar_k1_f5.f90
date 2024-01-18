!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ichar_k1_f5.f
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
!*  DESCRIPTION                : test ichar functionality with kind specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           character (4) c1
           c1 = '^'

	   k1 = kind(ichar(c1, -1+3-1));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(ichar(c1, kind = '10'b));
           if (k2 .ne. 2) error stop 2

           r1 = ichar(c1, O'10');
	   if (r1 .ne. 94) error stop 3

           r2 = ichar(c1, kind = 1*1);
	   if (r2 .ne. 94) error stop 4

END
