!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test len functionality with kind = 4
!*                               specified (with and without keyword),
!*                               no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (255) abc
           abc = '      &(*#iwjd8734         '

           k1 = kind(len(abc));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(len(abc , 4));
           if (k2 .ne. 4) error stop 2

           k3 = kind(len(abc , kind = 4));
           if (k3 .ne. 4) error stop 3

	   r1 = len(abc);
           if (r1 .ne. 255) error stop 4

	   r2 = len(abc , 4);
           if (r2 .ne. 255) error stop 5

	   r3 = len(abc , kind = 4);
           if (r3 .ne. 255) error stop 6

END
