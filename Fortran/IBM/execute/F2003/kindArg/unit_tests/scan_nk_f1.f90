!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : scan_nk_f1.f
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
!*  DESCRIPTION                : test scan functionality without kind
!*                               specified, with various combos of opt 
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2

           k1 = kind(scan('#)($Wrfewfe', 'jsklf3'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(scan('LAaw2sw13KJw2#UW41F', 'UW', BACK = .FALSE.));
           if (k2 .ne. 4) error stop 2

	   r1 = scan('Dodo', 'o');
           if (r1 .ne. 2) error stop 3

	   r2 = scan('Te08Ya6swiahw*#(', 'swia', .TRUE.);
           if (r2 .ne. 13) error stop 4

END
