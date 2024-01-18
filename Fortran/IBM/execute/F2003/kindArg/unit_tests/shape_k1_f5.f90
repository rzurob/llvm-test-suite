!*  ===================================================================
!*
!*  TEST CASE NAME             : shape_k1_f5.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test shape functionality with various kinds
!*                               specified (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(1), r2(1), r3(1)
           REAL COW (2:5)

           k1 = kind(shape(COW, kind = -9999+10000));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(shape(COW, kind = Z'0010'-8));
           if (k2 .ne. 8) error stop 2

	   k3 = kind(shape(COW, '1000'b));
           if (k3 .ne. 8) error stop 3

	   r1 = shape(COW, kind = 4*4/4+4-4);
           if (r1(1) .ne. 4) error stop 4

	   r2 = shape(COW, kind = '0001'b);
           if (r2(1) .ne. 4) error stop 5

	   r3 = shape(COW, O'1');
           if (r3(1) .ne. 4) error stop 6

END
