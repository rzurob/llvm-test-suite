!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test scan functionality with various
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(scan('KJasfaFK8234f', 'TR'));
           if (k1 .ne. 4) error stop 1

           k2 = kind(scan('i8092q3rijfwf;s][/.', 'TR', BACK = .TRUE.));
           if (k2 .ne. 4) error stop 2

           k2 = kind(scan('|/..kas{IEWIhsafiuafa', 'TR', kind = 2));
           if (k2 .ne. 2) error stop 3

           k4 = kind(scan('kjskdlJSkfsy7f8WO32uEWfwsjd', 'TR', BACK = .TRUE., kind = 2));
           if (k4 .ne. 2) error stop 4

           k5 = kind(scan('SDF>KJ8we&SF', 'TR', .TRUE., 2));
           if (k5 .ne. 2) error stop 5

           r1 = scan('ABCabcABC', 'abc');
           if (r1 .ne.4) error stop 6

	   r2 = scan('cdefefefe', 'fe', BACK = .FALSE.);
           if (r2 .ne. 3) error stop 7

	   r3 = scan('4135855', '55', kind = 2);
           if (r3 .ne. 4) error stop 8

	   r4 = scan('TesT', 'T', BACK = .FALSE., kind = 2);
           if (r4 .ne. 1) error stop 9

	   r5 = scan('NAN', 'NAN', .FALSE., 2);
           if (r5 .ne. 1) error stop 10

END
