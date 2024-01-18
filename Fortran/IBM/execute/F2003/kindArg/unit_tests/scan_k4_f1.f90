!*  ===================================================================
!*
!*  TEST CASE NAME             : scan_k4_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test scan functionality with various
!*                               combos of opt args, kind = 4 specified
!*                               (with and without keyword), -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(scan('K$(I%WH8w3fs', 'KSA'));
           if (k1 .ne. 8) error stop 1_4

           k2 = kind(scan('*Q*Q&^#', 'Q*Q', BACK = .TRUE.));
           if (k2 .ne. 8) error stop 2_4

           k2 = kind(scan('S)#$QkD8dKfs2', 'fs', kind = 4));
           if (k2 .ne. 4) error stop 3_4

 	   k4 = kind(scan('":}}{S&#($#', '$', BACK = .TRUE., kind = 4));
           if (k4 .ne. 4) error stop 4_4

 	   k5 = kind(scan('LKSJDFI*salf*', 'salf', .TRUE., 4));
           if (k5 .ne. 4) error stop 5_4

           r1 = scan('&@(#$HiHIHhi', 'HI');
           if (r1 .ne. 6) error stop 6_4

	   r2 = scan('ChiHUAHu@', 'Hu', BACK = .FALSE.);
           if (r2 .ne. 4) error stop 7_4

 	   r3 = scan('Pom', 'POM', kind = 4);
           if (r3 .ne. 1) error stop 8_4

	   r4 = scan('P455I747UDsjf2', '747', BACK = .FALSE., kind = 4);
           if (r4 .ne. 2) error stop 9_4

           ! right most character
           r5 = scan('FOR', 'OR', .TRUE., 4);
           if (r5 .ne. 3) error stop 10_4

END
