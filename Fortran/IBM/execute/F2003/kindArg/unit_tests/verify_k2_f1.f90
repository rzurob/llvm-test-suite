!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : verify_k2_f1.f
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
!*  DESCRIPTION                : test verify functionality with various 
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(verify('WI#(@HFHASF', 'A'));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(verify('ABCABCABC', 'ABC', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k3 = kind(verify('hijklmnhihihi', 'hi', Kind = 2));
           if (k3 .ne. 2) error stop 3

	   k4 = kind(verify('^%*&%^$*#*#*#*#', '*#', BACK = .TRUE., Kind = 2));
           if (k4 .ne. 2) error stop 4

	   k5 = kind(verify('xyzabc', 'abc', .TRUE., KIND = 2));
           if (k5 .ne. 2) error stop 5

	   r1 = verify('hijklmnhihihi', 'hi');
           if (r1 .ne. 3) error stop 6

	   r2 = verify('^%*&%^#*#*#*#*#', '*#', BACK = .TRUE.);
           if (r2 .ne. 6) error stop 7

	   r3 = verify('$99.99$', '99', Kind = 2);
           if (r3 .ne. 1) error stop 8

	   r4 = verify('LEVEL', 'LE', BACK = .TRUE., Kind = 2);
           if (r4 .ne. 3) error stop 9

	   r5 = verify('mamamam', 'mam', .FALSE., KIND = 2);
           if (r5 .ne. 0) error stop 10

END
