!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test shape functionality with kind = 4
!*                               specified (with and without keyword),
!*                               -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(3), r2(3), r3(3)
           logical COW (3:6, 1:2, 5:10)

           k1 = kind(shape(COW));
           if (k1 .ne. 8) error stop 1

	   k2 = kind(shape(COW, 4));
           if (k2 .ne. 4) error stop 2

           k3 = kind(shape(COW , kind = 4));
           if (k3 .ne. 4) error stop 3

	   r1 = shape(COW);
           if (r1(1) .ne. 4) error stop 4_1
           if (r1(2) .ne. 2) error stop 4_2
           if (r1(3) .ne. 6) error stop 4_3

	   r2 = shape(COW , 4);
           if (r2(1) .ne. 4) error stop 5_1
           if (r2(2) .ne. 2) error stop 5_2
           if (r2(3) .ne. 6) error stop 5_3

	   r3 = shape(COW , kind = 4);
           if (r3(1) .ne. 4) error stop 6_1
           if (r3(2) .ne. 2) error stop 6_2
           if (r3(3) .ne. 6) error stop 6_3
END
