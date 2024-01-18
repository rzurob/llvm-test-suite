!*  ===================================================================
!*
!*  TEST CASE NAME             : minloc_k1_f5.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test minloc functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1(1), r2(1), r3(1)

           k1 = kind(minloc((/1, 2, 3, 4, 5/), kind = -8373+ 8374));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(minloc((/1, 2, 3, 4, 5/), 1, kind = '0001'b));
           if (k2 .ne. 1) error stop 2

	   k3 = kind(minloc((/1, 2, 3, 4, 5/), 1, .TRUE., O'10'));
           if (k3 .ne. 8) error stop 3

	   r1 = minloc((/1, 2, 3, 4, 5/), kind = -8373+ 8374);
           if (r1(1) .ne. 1) error stop 4

	   r2 = minloc((/5, 4, 3, 2, 1/), 1, kind = '0001'b);
           if (r2(1) .ne. 5) error stop 5

	   r3 = minloc((/11, 12, 13, 4, 5/), 1, .TRUE., kind = O'10');
           if (r3(1) .ne. 4) error stop 6

END
