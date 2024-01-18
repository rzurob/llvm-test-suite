!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test maxloc functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890!

           integer :: k1, k2, k3, r1(1), r2(1), r3(1)
           integer :: array(5) = (/5, 4, 3, 2, 1/)

           k1 = kind(maxloc(array));
           if (k1 .ne. 4) error stop 1

           k2 = kind(maxloc((/5, 4, 3, 2, 1/), 1));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(maxloc((/5, 4, 3, 2, 1/), 1, .TRUE.));
           if (k3 .ne. 4) error stop 3

	   r1 = maxloc(array);
           if (r1(1) .ne. 1) error stop 4

	   r2 = maxloc((/5, 4, 3, 11/), dim = 1);
           if (r2(1) .ne. 4) error stop 5

	   r3 = maxloc((/5, 4, 33, 2, 1/), dim = 1, MASK = .TRUE.);
           if (r3(1) .ne. 3) error stop 6
END
