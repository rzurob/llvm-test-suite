!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality with various
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(index('laskf37#88#', '#88#'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(index('0I4498*#&$SHFas', '$', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k2 = kind(index('12SD448d72j', '72', kind = 2));
           if (k2 .ne. 2) error stop 3

           k4 = kind(index('+_];s&#(Ws', ';s', BACK = .TRUE., kind = 2));
           if (k4 .ne. 2) error stop 4

           k5 = kind(index('LAlalalala', 'la', .TRUE., 2));
           if (k5 .ne. 2) error stop 5

           r1 = index('&slkjfsfah&#', '+_js');
           if (r1 .ne. 0) error stop 6

	   r2 = index('APpLE', 'PP', BACK = .FALSE.);
           if (r2 .ne. 0) error stop 7

	   r3 = index('Kiwiiiiii@@@@i', 'i', kind = 2);
           if (r3 .ne. 2) error stop 8

	   r4 = index('Ju1c3', 'c3', BACK = .FALSE., kind = 2);
           if (r4 .ne. 4) error stop 9

	   r5 = index('o0o0o0o0ooo', 'ooo', .FALSE., 2);
           if (r5 .ne. 9) error stop 10

END
