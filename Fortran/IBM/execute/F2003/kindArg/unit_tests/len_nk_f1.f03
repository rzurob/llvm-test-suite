!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test len functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (128) abc
           abc = 'ZKRIerue'

           k1 = kind(len(abc));
           if (k1 .ne. 4) error stop 1

	   r1 = len(abc);
           if (r1 .ne. 128) error stop 2


END
