!*  ===================================================================
!*
!*  TEST CASE NAME             : lbound_k1_f5.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test lbound functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           REAL A (2:7, -1:4)

           k1 = kind(lbound(A, kind = 984 -983));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(lbound(A, 1, kind = '0010'b));
           if (k2 .ne. 2) error stop 2

	   k3 = kind(lbound(A, 1, O'1'));
           if (k3 .ne. 1) error stop 3

	   r1 = lbound(A, dim = 2, kind = 984 -983);
           if (r1 .ne. -1) error stop 4

	   r2 = lbound(A, 1, kind = '0010'b);
           if (r2 .ne. 2) error stop 5

	   r3 = lbound(A, 1, O'1');
           if (r3 .ne. 2) error stop 6

END
