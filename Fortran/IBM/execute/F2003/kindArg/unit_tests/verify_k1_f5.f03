!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test verify functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3

           k1 = kind(verify('1234231hkjasf', 'hk', kind = -9999+ 10003));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(verify('JAwe23KFAFhjqr371', 'JA', BACK = .FALSE., kind = O'0001'));
           if (k2 .ne. 1) error stop 2

	   k3 = kind(verify('*(sjfdisfy32rq%!^#*@', '*(sj', .TRUE., '10'b));
           if (k3 .ne. 2) error stop 3

	   r1 = verify('@)@#33#&@@!%!*#', '@)@#33', kind = -74833+ 74834);
           if (r1 .ne. 8) error stop 4

	   r2 = verify('!2006060624', '60624', BACK = .TRUE., kind = '0001'b);
           if (r2 .ne. 1) error stop 5

	   r3 = verify('+-*/', '+-*/', .TRUE. , O'10');
           if (r3 .ne. 0) error stop 6

END
