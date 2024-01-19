!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test scan functionality with various
!*                               combos of opt args, kind = 1 specified
!*                               (with and without keyword), -qintsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(scan('HI*slmvie', 'vie'));
           if (k1 .ne. 2) error stop 1

           k2 = kind(scan('73w*(#hAj', 'hA', BACK = .TRUE.));
           if (k2 .ne. 2) error stop 2

           k2 = kind(scan('jsdKJ&#qhfaqf', '&#', kind = 1));
           if (k2 .ne. 1) error stop 3

  	   k4 = kind(scan('Q*$sO*UWfs2', 'UWf', BACK = .TRUE., kind = 1));
           if (k4 .ne. 1) error stop 4

           k5 = kind(scan('+_#wsl;shad', '+_', .TRUE., 1));
           if (k5 .ne. 1) error stop 5

           r1 = scan('TRain', 'TR');
           if (r1 .ne. 1) error stop 6

	   r2 = scan('@ppL3', 'L3', BACK = .FALSE.);
           if (r2 .ne. 4) error stop 7

	   r3 = scan('gRaP3', 'gR', kind = 1);
           if (r3 .ne. 1) error stop 8

	   r4 = scan('0RanG3', 'RanG', BACK = .FALSE., kind = 1);
           if (r4 .ne. 2) error stop 9

	   r5 = scan('BERRY', 'ERR', .FALSE., 1);
           if (r5 .ne. 2) error stop 10

END
