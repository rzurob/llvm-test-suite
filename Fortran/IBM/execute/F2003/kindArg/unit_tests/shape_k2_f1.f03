!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test shape functionality with kind = 2
!*                               specified (with and without keyword),
!*                               -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(1), r2(1), r3(1)
           integer COW (2:5)

           k1 = kind(shape(COW));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(shape(COW, 2));
           if (k2 .ne. 2) error stop 2

           k3 = kind(shape(COW , kind = 2));
           if (k3 .ne. 2) error stop 3

	   r1 = shape(COW);
           if (r1(1) .ne. 4) error stop 4

	   r2 = shape(COW , 2);
           if (r2(1) .ne. 4) error stop 5

	   r3 = shape(COW , kind = 2);
           if (r3(1) .ne. 4) error stop 6


END
