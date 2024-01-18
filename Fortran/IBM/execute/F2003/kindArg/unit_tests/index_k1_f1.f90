!*  ===================================================================
!*
!*  TEST CASE NAME             : index_k1_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality with various
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(index('72uehJS*3', '*8sdJhs'));
           if (k1 .ne. 2) error stop 1

           k2 = kind(index('plos;9u32', 'u32', BACK = .TRUE.));
           if (k2 .ne. 2) error stop 2

           k2 = kind(index('0ISFfk|}{SFSFSFDFS/', 'pso', kind = 1));
           if (k2 .ne. 1) error stop 3

	   k4 = kind(index('12344567902', '\][s', BACK = .TRUE., kind = 1));
           if (k4 .ne. 1) error stop 4

           k5 = kind(index('9059377362', '77', .TRUE., 1));
           if (k5 .ne. 1) error stop 5

           r1 = index('|{""S&(#', '|');
           if (r1 .ne. 1) error stop 6

           r2 = index('+{[sdofsfJOjs83', '[', BACK = .FALSE.);
           if (r2 .ne. 3) error stop 7

	   r3 = index(')S*DFJSKF232hswfsf', '232', kind = 1);
           if (r3 .ne. 10) error stop 8

           r4 = index('p=r*T/2', '/2', BACK = .FALSE., kind = 1);
           if (r4 .ne. 6) error stop 9

	   r5 = index('ksd&=\[[p294', '=', .FALSE., 1);
           if (r5 .ne. 5) error stop 10

END
