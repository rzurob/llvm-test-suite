!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1(1), r2(1)
           real :: array(5) = (/5, 4, 3, 2, 1/)

           k1 = kind(minloc((/5, 4, 3, 2, 1/)));
           if (k1 .ne. 4) error stop 1

           k2 = kind(minloc((/1/), 1));
           if (k2 .ne. 4) error stop 2

	   k2 = kind(minloc((/3, 2, 1/), 1, .TRUE.));
           if (k2 .ne. 4) error stop 3

	   r1 = minloc(array);
           if (r1(1) .ne. 5) error stop 4

	   r2 = minloc((/2, 4, 3, 11/), dim = 1);
           if (r2(1) .ne. 1) error stop 5

	   r2 = minloc((/5, 4, 33, 2, 11/), dim = 1, MASK = .TRUE.);
           if (r2(1) .ne. 4) error stop 6
END
