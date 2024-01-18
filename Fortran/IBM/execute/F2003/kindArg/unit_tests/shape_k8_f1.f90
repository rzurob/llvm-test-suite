!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : shape_k8_f1.f
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
!*  DESCRIPTION                : test shape functionality with kind = 8 
!*                               specified (with and without keyword), 
!*                               no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890!

           integer :: k1, k2, k3, r1(4), r2(4), r3(4)
           REAL COW (1:1, 2:3, 3:5, 4:7)

           k1 = kind(shape(COW));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(shape(COW, 8));
           if (k2 .ne. 8) error stop 2

           k3 = kind(shape(COW , kind = 8));
           if (k3 .ne. 8) error stop 3

	   r1 = shape(COW);
           if (r1(1) .ne. 1) error stop 4_1
           if (r1(2) .ne. 2) error stop 4_2
           if (r1(3) .ne. 3) error stop 4_3
           if (r1(4) .ne. 4) error stop 4_4

	   r2 = shape(COW , 8);
           if (r2(1) .ne. 1) error stop 5_1
           if (r2(2) .ne. 2) error stop 5_2
           if (r2(3) .ne. 3) error stop 5_3
           if (r2(4) .ne. 4) error stop 5_4

	   r3 = shape(COW , kind = 8);
           if (r3(1) .ne. 1) error stop 6_1
           if (r3(2) .ne. 2) error stop 6_2
           if (r3(3) .ne. 3) error stop 6_3
           if (r3(4) .ne. 4) error stop 6_4

END
