!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test iachar functionality without kind
!*                               specified, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890


           integer :: k1, r1, r2, r3
           character (32) c1, c2, c3
           c1 = achar(12)
           c2 = achar(1)
           c3 = achar(127)

           k1 = kind(iachar(c1));
           if (k1 .ne. 4) error stop 1

           r1 = iachar(c1);
	   if (r1 .ne. 12) error stop 2

           r2 = iachar(c2);
	   if (r2 .ne. 1) error stop 3

           r3 = iachar(c3);
	   if (r3 .ne. 127) error stop 4

END
