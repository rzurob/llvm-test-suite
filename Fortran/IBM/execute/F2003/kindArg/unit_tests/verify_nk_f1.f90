!*  ===================================================================
!*
!*  TEST CASE NAME             : verify_nk_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test verify functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2

           k1 = kind(verify('mirror', 'mirr'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(verify('%%%%%%%*#J*^$%%Hsaf', '%%', .TRUE.));
           if (k2 .ne. 4) error stop 2

	   r1 = verify('%%%%%%%*#J*^$%%Hsaf', '%%');
           if (r1 .ne. 8) error stop 3

	   r2 = verify('mirror', 'rr', BACK = .FALSE.);
           if (r2 .ne. 1) error stop 4

END
