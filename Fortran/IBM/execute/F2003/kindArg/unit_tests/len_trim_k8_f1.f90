!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test len_trim functionality with kind = 8
!*                               specified (with and without keyword),
!*                               -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (32) abc
           abc = '][3XYZ        '

	   k1 = kind(len_trim(abc));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(len_trim(abc , 8));
           if (k2 .ne. 8) error stop 2

           k3 = kind(len_trim(abc , kind = 8));
           if (k3 .ne. 8) error stop 3

	   r1 = len_trim(abc);
           if (r1 .ne. 6) error stop 4

	   r2 = len_trim(abc , 8);
           if (r2 .ne. 6) error stop 5

	   r3 = len_trim(abc , kind = 8);
           if (r3 .ne. 6) error stop 6

END
