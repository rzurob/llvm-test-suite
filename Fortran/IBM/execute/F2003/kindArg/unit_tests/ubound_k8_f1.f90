!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test ubound functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, r1(2), r2, r3, r4(2), r5, r6, r7
           REAL A (2:15, -1:5)

	   k1 = kind(ubound(A));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(ubound(A, 2));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(ubound(A, dim = 1));
           if (k3 .ne. 4) error stop 3

           k4 = kind(ubound(A, kind = 8));
           if (k4 .ne. 8) error stop 4

	   k5 = kind(ubound(A, 1, kind = 8));
           if (k5 .ne. 8) error stop 5

	   k6 = kind(ubound(A, 2, 8));
           if (k6 .ne. 8) error stop 6

	   k7 = kind(ubound(A, dim = 1, kind = 8));
           if (k7 .ne. 8) error stop 7

	   r1 = ubound(A);
           if (r1(1) .ne. 15) error stop 8_1
           if (r1(2) .ne. 5) error stop 8_2

	   r2 = ubound(A, 2);
           if (r2 .ne. 5) error stop 9

	   r3 = ubound(A, dim = 1);
           if (r3 .ne. 15) error stop 10

	   r4 = ubound(A, kind = 8);
           if (r4(1) .ne. 15) error stop 11_1
	   if (r4(2) .ne. 5) error stop 11_2

	   r5 = ubound(A, 1, kind = 8);
           if (r5 .ne. 15) error stop 12

	   r6 = ubound(A, 2, 8);
           if (r6 .ne. 5) error stop 13

	   r7 = ubound(A, dim = 2, kind = 8);
           if (r7 .ne. 5) error stop 14
END