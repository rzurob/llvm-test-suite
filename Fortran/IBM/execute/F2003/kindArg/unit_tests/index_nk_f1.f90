!*  ===================================================================
!*
!*  TEST CASE NAME             : index_nk_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2

           k1 = kind(index('#)($Wrfewfe', 'R'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(index('JCH*#kahfrJawt', '#des', BACK = .FALSE.));
           if (k2 .ne. 4) error stop 2

	   r1 = index('Dodo', '0');
           if (r1 .ne. 0) error stop 3

	   r2 = index('Te0swiahw*#', 'swi', .TRUE.);
           if (r2 .ne. 4) error stop 4

END
