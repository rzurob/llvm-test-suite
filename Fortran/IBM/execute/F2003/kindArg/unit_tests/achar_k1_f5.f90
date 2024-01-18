!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test achar functionality w/ kind=1
!*                               (with and without keyword)
!*                               using various scalar init expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2
           character*1 :: r1, r2

	   k1 = kind(achar(86, -1+2));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(achar(86, kind = Z'1'));
           if (k2 .ne. 1) error stop 2

           r1 = achar(95, '1'b);
	   if (r1 .ne. '_') error stop 3

           r2 = achar(95, kind = 99/99);
	   if (r2 .ne. '_') error stop 4

END
