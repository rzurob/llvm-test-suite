!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : size_k4_f1.f
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
!*  DESCRIPTION                : test size functionality with various 
!*                               combos of opt args, kind = 4 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, k6, k7, r1, r2, r3, r4, r5, r6, r7
           integer(4) :: A (2:4, 1:5)

	   k1 = kind(size(A));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(size(A, 2));
           if (k2 .ne. 2) error stop 2

	   k3 = kind(size(A, dim = 1));
           if (k3 .ne. 2) error stop 3

           k4 = kind(size(A, kind = 4));
           if (k4 .ne. 4) error stop 4

	   k5 = kind(size(A, 1, kind = 4));
           if (k5 .ne. 4) error stop 5

	   k6 = kind(size(A, 2, 4));
           if (k6 .ne. 4) error stop 6

	   k7 = kind(size(A, dim = 1, kind = 4));
           if (k7 .ne. 4) error stop 7

	   r1 = size(A);
           if (r1 .ne. 15) error stop 8

	   r2 = size(A, 2);
           if (r2 .ne. 5) error stop 9

	   r3 = size(A, dim = 1);
           if (r3 .ne. 3) error stop 10

	   r4 = size(A, kind = 4);
           if (r4 .ne. 15) error stop 11

	   r5 = size(A, 1, kind = 4);
           if (r5 .ne. 3) error stop 12

	   r6 = size(A, 2, 4);
           if (r6 .ne. 5) error stop 13

	   r7 = size(A, dim = 2, kind = 4);
           if (r7 .ne. 5) error stop 14


END
