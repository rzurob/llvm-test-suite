!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test maxloc functionality with various
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)

           k1 = kind(maxloc((/1, 2, 3/)));
           if (k1 .ne. 8) error stop 1_4

	   k2 = kind(maxloc((/1, 2, 3/), 1));
           if (k2 .ne. 8) error stop 2_4

	   k3 = kind(maxloc((/1, 2, 3/), 1, .TRUE.));
           if (k3 .ne. 8) error stop 3_4

           k4 = kind(maxloc((/1, 2, 3/), kind = 2));
           if (k4 .ne. 2) error stop 4_4

	   k5 = kind(maxloc((/1, 2, 3/), 1, kind = 2));
           if (k5 .ne. 2) error stop 5_4

	   k6 = kind(maxloc((/1, 2, 3/), 1, .TRUE., kind = 2));
           if (k6 .ne. 2) error stop 6_4

	   k7 = kind(maxloc((/1, 2, 3/), mask =.TRUE., kind = 2));
           if (k7 .ne. 2) error stop 7_4

	   k8 = kind(maxloc((/1, 2, 3/), mask =.FALSE.));
           if (k8 .ne. 8) error stop 8_4

	   r1 = maxloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 3) error stop 9_4

	   r2 = maxloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10_4

	   r3 = maxloc((/2, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 2) error stop 11_4

	   r4 = maxloc((/2, 6/), kind = 2);
           if (r4(1) .ne. 2) error stop 12_4

	   r5 = maxloc((/2, 6, 4, 1, 15/), 1, kind = 2);
           if (r5(1) .ne. 5) error stop 13_4

	   r6 = maxloc((/2, 6, 4, 11/), 1, .TRUE., kind = 2);
           if (r6(1) .ne. 4) error stop 14_4

	   r7 = maxloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 2);
           if (r7(1) .ne. 4) error stop 15_4

	   r8 = maxloc((/7, 6, 4, 7, 3/), mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16_4
END
