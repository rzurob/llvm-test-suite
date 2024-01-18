!*  ===================================================================
!*
!*  TEST CASE NAME             : shape_k1_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test shape functionality with kind = 1
!*                               specified (with and without keyword),
!*                               -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(2), r2(2), r3(2)
           character COW (1:4, 3:9)

           k1 = kind(shape(COW));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(shape(COW, 1));
           if (k2 .ne. 1) error stop 2

           k3 = kind(shape(COW , kind = 1));
           if (k3 .ne. 1) error stop 3

	   r1 = shape(COW);
           if (r1(1) .ne. 4) error stop 4_1
           if (r1(2) .ne. 7) error stop 4_2

	   r2 = shape(COW , 1);
           if (r2(1) .ne. 4) error stop 5_1
           if (r2(2) .ne. 7) error stop 5_2

	   r3 = shape(COW , kind = 1);
           if (r3(1) .ne. 4) error stop 6_1
           if (r3(2) .ne. 7) error stop 6_2
END
