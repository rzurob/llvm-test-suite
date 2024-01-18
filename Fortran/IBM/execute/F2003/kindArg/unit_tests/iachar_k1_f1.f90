!*  ===================================================================
!*
!*  TEST CASE NAME             : iachar_k1_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test iachar functionality with kind = 1
!*                               specified (with and without keyword),
!*                               -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           character (64) c1
           c1 = '.'

	   k1 = kind(iachar(c1, 1));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(iachar(c1));
           if (k2 .ne. 2) error stop 2

	   k3 = kind(iachar(c1, kind = 1));
           if (k3 .ne. 1) error stop 3

           r1 = iachar(c1, 1);
	   if (r1 .ne. 46) error stop 4

           r2 = iachar(c1);
	   if (r2 .ne. 46) error stop 5

           r3 = iachar(c1, kind =1);
	   if (r3 .ne. 46) error stop 6

END
