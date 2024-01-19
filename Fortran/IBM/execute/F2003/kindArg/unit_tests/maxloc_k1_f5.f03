!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test maxloc functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(1), r2(1), r3(1)
           integer :: array(9) = (/15, 4, 3, 2, 1, 46, 6, 8, 7/)

           k1 = kind(maxloc(array, kind = -2 + 3));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(maxloc((/198, 5/), 1, kind = '1'b));
           if (k2 .ne. 1) error stop 2

	   k3 = kind(maxloc((/1, 2, 3, 4, 5/), 1, .TRUE., Z'08'));
           if (k3 .ne. 8) error stop 3

	   r1 = maxloc((/3/), kind = 10 -14 + 5);
           if (r1(1) .ne. 1) error stop 4

	   r2 = maxloc((/5, 4, 3, 2, 1/), 1, kind = '1'b);
           if (r2(1) .ne. 1) error stop 5

	   r3 = maxloc(array, 1, .TRUE., kind = O'10');
           if (r3(1) .ne. 6) error stop 6

END
