!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ubound_nk_f1.f
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
!*  DESCRIPTION                : test ubound functionality without kind
!*                               specified, with various combos of opt 
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           REAL A (3:8 , 6:7)

           k1 = kind(ubound(A));
           if (k1 .ne. 4) error stop 1

           k2 = kind(ubound(A, 1));
           if (k2 .ne. 4) error stop 2

	   r1 = ubound(A, 2);
           if (r1 .ne. 7) error stop 3

	   r2 = ubound(A, dim = 1);
           if (r2 .ne. 8) error stop 4

END
