!*  ===================================================================
!*
!*  TEST CASE NAME             : len_k8_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test len functionality with kind = 8
!*                               specified (with and without keyword),
!*                               -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (32) abc
           abc = 'tuvXYZ'

	   k1 = kind(len(abc));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(len(abc , 8));
           if (k2 .ne. 8) error stop 2

           k3 = kind(len(abc , kind = 8));
           if (k3 .ne. 8) error stop 3

	   r1 = len(abc);
           if (r1 .ne. 32) error stop 4

	   r2 = len(abc , 8);
           if (r2 .ne. 32) error stop 5

	   r3 = len(abc , kind = 8);
           if (r3 .ne. 32) error stop 6

END
