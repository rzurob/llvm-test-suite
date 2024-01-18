!*  ===================================================================
!*
!*  TEST CASE NAME             : maxloc_k1_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test maxloc functionality with various
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)
           real :: array(10) = (/46, 15, 4, 3, 2, 1, 46, 6, 8, 7/)

           k1 = kind(maxloc((/4, 1, 5/)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(maxloc(array, 1));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(maxloc((/1836, 83, 3, 4, 5, 6, 7, 8/), 1, .TRUE.));
           if (k3 .ne. 4) error stop 3

           k4 = kind(maxloc((/613, 243, 4, 241, 24/), kind = 1));
           if (k4 .ne. 1) error stop 4

	   k5 = kind(maxloc((/8, 409, 293, 7, 2/), 1, kind = 1));
           if (k5 .ne. 1) error stop 5

	   k6 = kind(maxloc(array, 1, .TRUE., kind = 1));
           if (k6 .ne. 1) error stop 6

	   k7 = kind(maxloc((/12, 26, 234, 13, 503/), mask =.TRUE., kind = 1));
           if (k7 .ne. 1) error stop 7

	   k8 = kind(maxloc((/32, 272, 873, 492, 102, 573/), mask =.FALSE.));
           if (k8 .ne. 4) error stop 8

	   r1 = maxloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 3) error stop 9

	   r2 = maxloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10

	   r3 = maxloc(array, 1, .TRUE.);
           if (r3(1) .ne. 1) error stop 11

	   r4 = maxloc((/2, 6/), kind = 1);
           if (r4(1) .ne. 2) error stop 12

	   r5 = maxloc((/2, 6, 4, 1, 15/), 1, kind = 1);
           if (r5(1) .ne. 5) error stop 13

	   r6 = maxloc((/2, 6, 4, 11/), 1, .TRUE., kind = 1);
           if (r6(1) .ne. 4) error stop 14

	   r7 = maxloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 1);
           if (r7(1) .ne. 4) error stop 15

	   r8 = maxloc(array, mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16
END
