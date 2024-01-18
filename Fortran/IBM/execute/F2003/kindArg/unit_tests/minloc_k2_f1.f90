!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : minloc_k2_f1.f
!*
!*  PROGRAMMER                 : Vince Yuen
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality with various 
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, k8, r1(1), r2(1), r3(1), r4(1), r5(1), r6(1), r7(1), r8(1)
           real :: array(5) = (/5, 4, 3, 2, 1/)

           k1 = kind(minloc((/2, 16, 64, 1, 5, 4, 2/)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(minloc((/4, 3, 6/), 1));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(minloc((/2, 6, 4, 1, 5/), 1, .TRUE.));
           if (k3 .ne. 4) error stop 3

           k4 = kind(minloc((/98, 36, 31, 11, 5/), kind = 2));
           if (k4 .ne. 2) error stop 4

	   k5 = kind(minloc((/23, 1, 5/), 1, kind = 2));
           if (k5 .ne. 2) error stop 5

	   k6 = kind(minloc((/647, 634 /), 1, .TRUE., kind = 2));
           if (k6 .ne. 2) error stop 6

	   k7 = kind(minloc((/83, 6, 14, 1, 215/), mask =.TRUE., kind = 2));
           if (k7 .ne. 2) error stop 7

	   k8 = kind(minloc((/212, 13, 4, 2, 5/), mask =.FALSE.));
           if (k8 .ne. 4) error stop 8

	   r1 = minloc((/2, 4, 6, 1, 5/));
           if (r1(1) .ne. 4) error stop 9

	   r2 = minloc((/8/), 1);
           if (r2(1) .ne. 1) error stop 10

	   r3 = minloc((/1, 6, 4, 1, 5/), 1, .TRUE.);
           if (r3(1) .ne. 1) error stop 11

	   r4 = minloc((/12, 6, 4/), kind = 2);
           if (r4(1) .ne. 3) error stop 12

	   r5 = minloc(array, 1, kind = 2);
           if (r5(1) .ne. 5) error stop 13

	   r6 = minloc(array, 1, .TRUE., kind = 2);
           if (r6(1) .ne. 5) error stop 14

	   r7 = minloc((/3, 6, 4, 9, 5, 8, 1/), mask = .TRUE., kind = 2);
           if (r7(1) .ne. 7) error stop 15

	   r8 = minloc((/7, 6, 4, 7, 3/), mask =.FALSE.);
           if (r8(1) .ne. 0) error stop 16
END
