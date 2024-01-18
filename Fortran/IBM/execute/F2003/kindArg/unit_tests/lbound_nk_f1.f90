!*  ===================================================================
!*
!*  TEST CASE NAME             : lbound_nk_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test lbound functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           REAL A (3:8, 2:5)

           k1 = kind(lbound(A));
           if (k1 .ne. 4) error stop 1

           k2 = kind(lbound(A, 1));
           if (k2 .ne. 4) error stop 2

	   r1 = lbound(A, 2);
           if (r1 .ne. 2) error stop 3

	   r2 = lbound(A, dim = 1);
           if (r2 .ne. 3) error stop 4

END
