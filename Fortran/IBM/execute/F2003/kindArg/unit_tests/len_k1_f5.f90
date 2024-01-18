!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : len_k1_f5.f
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
!*  DESCRIPTION                : test len functionality with various kinds
!*                               specified (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (129) abc
           abc = 'aBc123457'

           k1 = kind(len(abc, kind = -8373+ 8374));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(len(abc, kind = '0001'b));
           if (k2 .ne. 1) error stop 2

	   k3 = kind(len(abc, O'10'));
           if (k3 .ne. 8) error stop 3

	   r1 = len(abc, kind = -8373+ 8374);
           if (r1 .ne. 129) error stop 4

	   r2 = len(abc, kind = '0001'b);
           if (r2 .ne. 129) error stop 5

	   r3 = len(abc, O'10');
           if (r3 .ne. 129) error stop 6

END
