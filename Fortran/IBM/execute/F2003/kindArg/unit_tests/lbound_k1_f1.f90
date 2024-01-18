!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : lbound_k1_f1.f
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
!*  DESCRIPTION                : test lbound functionality with various 
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, r1(3), r2, r3, r4(3), r5, r6, r7
           REAL A (2:5, -1:5, 12:127)

	   k1 = kind(lbound(A));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(lbound(A, 2));
           if (k2 .ne. 2) error stop 2

	   k3 = kind(lbound(A, dim = 1));
           if (k3 .ne. 2) error stop 3

           k4 = kind(lbound(A, kind = 1));
           if (k4 .ne. 1) error stop 4

	   k5 = kind(lbound(A, 1, kind = 1));
           if (k5 .ne. 1) error stop 5

	   k6 = kind(lbound(A, 2, 1));
           if (k6 .ne. 1) error stop 6

	   k7 = kind(lbound(A, dim = 1, kind = 1));
           if (k7 .ne. 1) error stop 7

	   r1 = lbound(A);
           if (r1(1) .ne. 2) error stop 8_1
           if (r1(2) .ne. -1) error stop 8_2
           if (r1(3) .ne. 12) error stop 8_3

	   r2 = lbound(A, 2);
           if (r2 .ne. -1) error stop 9

	   r3 = lbound(A, dim = 1);
           if (r3 .ne. 2) error stop 10

	   r4 = lbound(A, kind = 1);
           if (r4(1) .ne. 2) error stop 11_1
	   if (r4(2) .ne. -1) error stop 11_2
           if (r4(3) .ne. 12) error stop 11_3

	   r5 = lbound(A, 1, kind = 1);
           if (r5 .ne. 2) error stop 12

	   r6 = lbound(A, 2, 1);
           if (r6 .ne. -1) error stop 13

	   r7 = lbound(A, dim = 2, kind = 1);
           if (r7 .ne. -1) error stop 14

END
