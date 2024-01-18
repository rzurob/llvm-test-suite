!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test scan functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(scan('ABC', 'A'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(scan('xyz', 'z', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k2 = kind(scan('abcXYZ', 'XY', kind = 8));
           if (k2 .ne. 8) error stop 3

           k4 = kind(scan('123456789', '123', BACK = .TRUE., kind = 8));
           if (k4 .ne. 8) error stop 4

           k5 = kind(scan('!#%&(', '{}', .TRUE., 8));
           if (k5 .ne. 8) error stop 5

           r1 = scan('JCH*#kahfrJawt!', 'kah');
           if (r1 .ne. 6) error stop 6

	   r2 = scan('}{|:">', '{}', BACK = .FALSE.);
           if (r2 .ne. 1) error stop 7

	   r3 = scan('OIadfgSs32gF', 'ss', kind = 8);
           if (r3 .ne. 8) error stop 8

	   r4 = scan('Geeesnz8', 'eee', BACK = .TRUE., kind = 8);
           if (r4 .ne. 4) error stop 9

	   r5 = scan('Oisjahsw4e4s', '4e4', .FALSE., 8);
           if (r5 .ne. 9) error stop 10

END
