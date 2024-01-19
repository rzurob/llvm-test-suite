!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test verify functionality with various
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer (8) :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(verify('SAFIEJBCA', 'B'));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(verify('orange', 'r', BACK = .TRUE.));
           if (k2 .ne. 2) error stop 2

           k3 = kind(verify('ksdjfksauqqdo', 'j', Kind = 1));
           if (k3 .ne. 1) error stop 3

	   k4 = kind(verify('ALSkSyWew', 'L', BACK = .TRUE., Kind = 1));
           if (k4 .ne. 1) error stop 4

	   k5 = kind(verify('ABBccccA', 'A', .TRUE., KIND = 1));
           if (k5 .ne. 1) error stop 5

	   r1 = verify('SAFIEJBCA', 'SAFIE');
           if (r1 .ne. 6) error stop 6

	   r2 = verify('yOu', 'y0', BACK = .TRUE.);
           if (r2 .ne. 3) error stop 7

	   r3 = verify('&@*&#83', '&@', Kind = 1);
           if (r3 .ne. 3) error stop 8

	   r4 = verify('GraP3', 'ra', BACK = .TRUE., Kind = 1);
           if (r4 .ne. 5) error stop 9

	   r5 = verify('r@nd0m', 'r@nd', .FALSE., KIND = 1);
           if (r5 .ne. 5) error stop 10
END
