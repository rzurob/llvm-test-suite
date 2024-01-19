!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test verify functionality with various
!*                               combos of opt args, kind = 4 specified
!*                               (with and without keyword), -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(verify('&*@UIHEKJHAF', 'H#UR'));
           if (k1 .ne. 8) error stop 1

	   k2 = kind(verify('];LJ(,./[HHFRKEAF', 'EA', BACK = .TRUE.));
           if (k2 .ne. 8) error stop 2

           k3 = kind(verify(';:.,;;',';;', Kind = 4));
           if (k3 .ne. 4) error stop 3

	   k4 = kind(verify('OOOOoooo', 'oo', BACK = .TRUE., Kind = 4));
           if (k4 .ne. 4) error stop 4

	   k5 = kind(verify('965487569', '965', .TRUE., KIND = 4));
           if (k5 .ne. 4) error stop 5

	   r1 = verify(';:.,;;', ';');
           if (r1 .ne. 2) error stop 6

	   r2 = verify('965487569', '965', BACK = .TRUE.);
           if (r2 .ne. 6) error stop 7

	   r3 = verify('OOOOoooo', 'OOoo', Kind = 8);
           if (r3 .ne. 5) error stop 8

	   r4 = verify('BOBOBOB', 'BOB', BACK = .TRUE., Kind = 8);
           if (r4 .ne. 4) error stop 9

	   r5 = verify('&124#42^27711', '7711', .FALSE., KIND = 8);
           if (r5 .ne. 1) error stop 10

END
