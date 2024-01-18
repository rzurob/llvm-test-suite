!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : verify_k8_f1.f
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
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(verify('POPopOP', 'POP'));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(verify('&":SDFiHEMFDA(@', '&"', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k3 = kind(verify('plSDo*s;.djwe', 'plS', Kind = 8));
           if (k3 .ne. 8) error stop 3

	   k4 = kind(verify('}{OWh;&^ksdf(@KSD', '}{', BACK = .TRUE., Kind = 8));
           if (k4 .ne. 8) error stop 4

	   k5 = kind(verify('02j;74jr#pwdJDSwd832s', '02j;', .TRUE., KIND = 8));
           if (k5 .ne. 8) error stop 5

	   r1 = verify('POPopOP', 'POP');
           if (r1 .ne. 4) error stop 6

	   r2 = verify('Wh0', 'WhO', BACK = .TRUE.);
           if (r2 .ne. 3) error stop 7

	   r3 = verify('aTTatTat', 'ATT', Kind = 8);
           if (r3 .ne. 1) error stop 8

	   r4 = verify('N^*#&N^', 'N^', BACK = .TRUE., Kind = 8);
           if (r4 .ne. 5) error stop 9

	   r5 = verify('H', 'H', .FALSE., KIND = 8);
           if (r5 .ne. 0) error stop 10

END
