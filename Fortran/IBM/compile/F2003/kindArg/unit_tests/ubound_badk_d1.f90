!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test error with kind =an invalid value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, r1, r2
           REAL A (2:8, -1:5)

	   r1 = ubound(A, 2);
           if (r1 .ne. 5) error stop 1

	   k1 = kind(ubound(A, kind = r1));

	   r2 = ubound(A, kind = 989);
END
