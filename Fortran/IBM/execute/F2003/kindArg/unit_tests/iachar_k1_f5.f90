!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test iachar functionality with kind specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           character (4) c1
           c1 = '%'

	   k1 = kind(iachar(c1, -1+3-1));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(iachar(c1, kind = '1000'b));
           if (k2 .ne. 8) error stop 2

           r1 = iachar(c1, Z'0000004');
	   if (r1 .ne. 37) error stop 3

           r2 = iachar(c1, kind = 1*1+1-1/1);
	   if (r2 .ne. 37) error stop 4

END
