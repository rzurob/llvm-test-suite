!*  ===================================================================
!*
!*  TEST CASE NAME             : minloc_k4_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality with various
!*                               combos of opt args, kind = 4 specified
!*                               (with and without keyword), -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)
           integer :: array(5) = (/5, 4, 3, 2, 1/)

           k1 = kind(minloc((/23, 62, 14, 82, 72, 43/)));
           if (k1 .ne. 8) error stop 1_4

	   k2 = kind(minloc((/21, 26, 44, 71, 500, 3, 4, 6, 23/), 1));
           if (k2 .ne. 8) error stop 2_4

	   k3 = kind(minloc((/23, 63, 534, 13, 3, 5, 7/), 1, .TRUE.));
           if (k3 .ne. 8) error stop 3_4

           k4 = kind(minloc((/74, 92, 84, 31, 5, 34/), kind = 4));
           if (k4 .ne. 4) error stop 4_4

	   k5 = kind(minloc((/2, 1, 5/), 1, kind = 4));
           if (k5 .ne. 4) error stop 5_4

	   k6 = kind(minloc((/222, 66, 44, 11/), 1, .TRUE., kind = 4));
           if (k6 .ne. 4) error stop 6_4

	   k7 = kind(minloc((/22, 43, 98, 18/), mask =.TRUE., kind = 4));
           if (k7 .ne. 4) error stop 7_4

	   k8 = kind(minloc((/21, 22, 412, 231, 355, 3, 1, 0/), mask =.FALSE.));
           if (k8 .ne. 8) error stop 8_4

	   r1 = minloc(array);
           if (r1(1) .ne. 5) error stop 9_4

	   r2 = minloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10_4

	   r3 = minloc((/1, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 1) error stop 11_4

	   r4 = minloc(array, kind = 4);
           if (r4(1) .ne. 5) error stop 12_4

	   r5 = minloc((/2, 6, 4, 11, 15/), 1, kind = 4);
           if (r5(1) .ne. 1) error stop 13_4

	   r6 = minloc((/12, 6, 14, 11/), 1, .TRUE., kind = 4);
           if (r6(1) .ne. 2) error stop 14_4

	   r7 = minloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 4);
           if (r7(1) .ne. 7) error stop 15_4

	   r8 = minloc(array, mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16_4
END

