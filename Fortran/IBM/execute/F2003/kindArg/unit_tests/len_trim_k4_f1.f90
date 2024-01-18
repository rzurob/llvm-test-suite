!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : len_trim_k4_f1.f
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
!*  DESCRIPTION                : test len_trim functionality with kind = 4
!*                               specified (with and without keyword),
!*                               -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (16) abc
           abc = '123XYZABCabcDEFg'

           k1 = kind(len_trim(abc));
           if (k1 .ne. 8) error stop 1

	   k2 = kind(len_trim(abc , 4));
           if (k2 .ne. 4) error stop 2

           k3 = kind(len_trim(abc , kind = 4));
           if (k3 .ne. 4) error stop 3

	   r1 = len_trim(abc);
           if (r1 .ne. 16) error stop 4

	   r2 = len_trim(abc , 4);
           if (r2 .ne. 16) error stop 5

	   r3 = len_trim(abc , kind = 4);
           if (r3 .ne. 16) error stop 6

END
