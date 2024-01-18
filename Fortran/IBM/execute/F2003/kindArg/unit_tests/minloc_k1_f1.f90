!*  ===================================================================
!*
!*  TEST CASE NAME             : minloc_k1_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality with various
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)

           k1 = kind(minloc((/2, 6, 4, 1, 5/)));
           if (k1 .ne. 2) error stop 1_4

       k2 = kind(minloc((/52, 6, 54, 1, 55/), 1));
           if (k2 .ne. 2) error stop 2_4

       k3 = kind(minloc((/32, 46, 44, 31, 52/), 1, .TRUE.));
           if (k3 .ne. 2) error stop 3_4

           k4 = kind(minloc((/2, 16, 4, 1, 5/), kind = 1));
           if (k4 .ne. 1) error stop 4_4

       k5 = kind(minloc((/2, 6, 4, 1, 5/), 1, kind = 1));
           if (k5 .ne. 1) error stop 5_4

       k6 = kind(minloc((/2, 536, 4, 1, 35/), 1, .TRUE., kind = 1));
           if (k6 .ne. 1) error stop 6_4

       k7 = kind(minloc((/12, 26, 36, 1, 55/), mask =.TRUE., kind = 1));
           if (k7 .ne. 1) error stop 7_4

       k8 = kind(minloc((/1232, 634, 425, 12, 98/), mask =.FALSE.));
           if (k8 .ne. 2) error stop 8_4

       r1 = minloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 4) error stop 9_4

       r2 = minloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10_4

       r3 = minloc((/1, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 1) error stop 11_4

       r4 = minloc((/12, 6/), kind = 1);
           if (r4(1) .ne. 2) error stop 12_4

       r5 = minloc((/2, 6, 4, 11, 15/), 1, kind = 1);
           if (r5(1) .ne. 1) error stop 13_4

       r6 = minloc((/12, 6, 14, 11/), 1, .TRUE., kind = 1);
           if (r6(1) .ne. 2) error stop 14_4

       r7 = minloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 1);
           if (r7(1) .ne. 7) error stop 15_4

       r8 = minloc((/7, 6, 4, 7, 3/), mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16_4
END
