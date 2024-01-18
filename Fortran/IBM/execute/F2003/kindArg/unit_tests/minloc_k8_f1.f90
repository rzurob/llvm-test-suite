!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)

           k1 = kind(minloc((/1, 26, 42, 31, 15/)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(minloc((/2/), 1));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(minloc((/372, 93, 11, 53/), 1, .TRUE.));
           if (k3 .ne. 4) error stop 3

           k4 = kind(minloc((/847, 123, 13, 5, 928/), kind = 8));
           if (k4 .ne. 8) error stop 4

	   k5 = kind(minloc((/27, 95, 85, 52, 8, 2, 5, 87, 90/), 1, kind = 8));
           if (k5 .ne. 8) error stop 5

	   k6 = kind(minloc((/75, 63, 41, 18, 55, 32, 98/), 1, .TRUE., kind = 8));
           if (k6 .ne. 8) error stop 6

	   k7 = kind(minloc((/12, 16, 6, 549/), mask =.TRUE., kind = 8));
           if (k7 .ne. 8) error stop 7

	   k8 = kind(minloc((/1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/), mask =.FALSE.));
           if (k8 .ne. 4) error stop 8

	   r1 = minloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 4) error stop 9

	   r2 = minloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10

	   r3 = minloc((/1, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 1) error stop 11

	   r4 = minloc((/12, 6/), kind = 8);
           if (r4(1) .ne. 2) error stop 12

	   r5 = minloc((/2, 6, 4, 11, 1/), 1, kind = 8);
           if (r5(1) .ne. 5) error stop 13

	   r6 = minloc((/12, 6, 14, 11/), 1, .TRUE., kind = 8);
           if (r6(1) .ne. 2) error stop 14

	   r7 = minloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 8);
           if (r7(1) .ne. 7) error stop 15

	   r8 = minloc((/7, 6, 4, 7, 3/), mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16


END