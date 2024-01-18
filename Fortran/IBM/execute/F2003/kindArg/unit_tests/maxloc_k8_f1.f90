!*  ===================================================================
!*
!*  TEST CASE NAME             : maxloc_k8_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test maxloc functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)
           integer :: array(5) = (/5, 4, 3, 2, 1/)

           k1 = kind(maxloc((/2, 6, 4, 1, 5/)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(maxloc(array, 1));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(maxloc((/12, 56, 4, 1, 5/), 1, .TRUE.));
           if (k3 .ne. 4) error stop 3

           k4 = kind(maxloc((/2, 6, 4, 1, 5/), kind = 8));
           if (k4 .ne. 8) error stop 4

	   k5 = kind(maxloc((/482, 66, 4, 1, 5, 73, 8/), 1, kind = 8));
           if (k5 .ne. 8) error stop 5

	   k6 = kind(maxloc(array, 1, .TRUE., kind = 8));
           if (k6 .ne. 8) error stop 6

	   k7 = kind(maxloc((/2, 6, 4, 1, 5/), mask =.TRUE., kind = 8));
           if (k7 .ne. 8) error stop 7

	   k8 = kind(maxloc(array, mask =.FALSE.));
           if (k8 .ne. 4) error stop 8

	   r1 = maxloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 3) error stop 9

	   r2 = maxloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10

	   r3 = maxloc((/2, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 2) error stop 11

	   r4 = maxloc(array, kind = 8);
           if (r4(1) .ne. 1) error stop 12

	   r5 = maxloc((/2, 6, 4, 1, 15/), 1, kind = 8);
           if (r5(1) .ne. 5) error stop 13

	   r6 = maxloc((/2, 6, 4, 11/), 1, .TRUE., kind = 8);
           if (r6(1) .ne. 4) error stop 14

	   r7 = maxloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 8);
           if (r7(1) .ne. 4) error stop 15

	   r8 = maxloc(array, mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16

END