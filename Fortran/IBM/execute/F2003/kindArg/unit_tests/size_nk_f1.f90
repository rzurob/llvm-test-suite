!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test size functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           character A (6:8, 2:4, -10:10)

           k1 = kind(size(A));
           if (k1 .ne. 4) error stop 1

           k2 = kind(size(A, 1));
           if (k2 .ne. 4) error stop 2

	   r1 = size(A);
           if (r1 .ne. 189) error stop 3

	   r2 = size(A, dim = 3);
           if (r2 .ne. 21) error stop 4

END
