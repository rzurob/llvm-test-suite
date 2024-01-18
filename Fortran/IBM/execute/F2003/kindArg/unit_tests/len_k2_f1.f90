!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : len_k2_f1.f
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
!*  DESCRIPTION                : test len functionality with kind = 2
!*                               specified (with and without keyword),
!*                               -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (64) abc
           abc = 'upupupupupup'

           k1 = kind(len(abc));
           if (k1 .ne. 8) error stop 1

	   k2 = kind(len(abc , 2));
           if (k2 .ne. 2) error stop 2

           k3 = kind(len(abc , kind = 2));
           if (k3 .ne. 2) error stop 3

	   r1 = len(abc);
           if (r1 .ne. 64) error stop 4

	   r2 = len(abc , 2);
           if (r2 .ne. 64) error stop 5

	   r3 = len(abc , kind = 2);
           if (r3 .ne. 64) error stop 6

END
