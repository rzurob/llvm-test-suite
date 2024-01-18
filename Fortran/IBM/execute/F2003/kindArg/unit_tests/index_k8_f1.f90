!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(index('82hsfsa', 's'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(index('abcXyZ123', 'xyz', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k2 = kind(index('0932":DS2', 'DS', kind = 8));
           if (k2 .ne. 8) error stop 3

           k4 = kind(index('&jsdf27sEIW', '27', BACK = .TRUE., kind = 8));
           if (k4 .ne. 8) error stop 4

           k5 = kind(index('0s[][^*(#fa13', '[]', .TRUE., 8));
           if (k5 .ne. 8) error stop 5

           r1 = index('0s[][^*(#fa13', '[]');
           if (r1 .ne. 3) error stop 6

	   r2 = index('FORTRAN', 'R', BACK = .FALSE.);
           if (r2 .ne. 3) error stop 7

 	   r3 = index('}{|:">', '{}', kind = 8);
           if (r3 .ne. 0) error stop 8

	   r4 = index('Geeesnz8', 'ee', BACK = .TRUE., kind = 8);
           if (r4 .ne. 3) error stop 9

	   r5 = index('ChiHUAHu@', 'Hu', .FALSE., 8);
           if (r5 .ne. 7) error stop 10

END
