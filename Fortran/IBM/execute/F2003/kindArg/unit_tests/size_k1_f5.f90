!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test size functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           REAL A (2:15, 1:5)

           k1 = kind(size(A, kind = -4*2 + 9));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(size(A, 1, kind = '0010'b));
           if (k2 .ne. 2) error stop 2

	   k3 = kind(size(A, 1, O'10'));
           if (k3 .ne. 8) error stop 3

	   r1 = size(A, dim = 2, kind = 85 - 84*1);
           if (r1 .ne. 5) error stop 4

	   r2 = size(A, 1, kind = '0001'b);
           if (r2 .ne. 14) error stop 5

	   r3 = size(A, 1, O'10');
           if (r3 .ne. 14) error stop 6

END
