!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test iachar functionality with kind = 8
!*                               specified (with and without keyword),
!*                               no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           character (64) c1
           c1 = 'Z'

	   k1 = kind(iachar(c1, 8));
           if (k1 .ne. 8) error stop 1

	   k2 = kind(iachar(c1, kind = 8));
           if (k2 .ne. 8) error stop 2

	   k3 = kind(iachar(c1));
           if (k3 .ne. 4) error stop 3

           r1 = iachar(c1, 8);
	   if (r1 .ne. 90) error stop 4

           r2 = iachar(c1, kind =8);
	   if (r2 .ne. 90) error stop 5

	   r3 = iachar(c1);
	   if (r3 .ne. 90) error stop 6

END