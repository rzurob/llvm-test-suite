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
           REAL COW (2:5)

	   r1 = size(COW);
           if (r1 .ne. 4) error stop 1

	   k1 = kind(size(COW, kind = r1));

	   r2 = size(COW, kind = -9);
END
