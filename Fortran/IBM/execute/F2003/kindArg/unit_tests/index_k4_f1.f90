!*  ===================================================================
!*
!*  TEST CASE NAME             : index_k4_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality with various
!*                               combos of opt args, kind = 4 specified
!*                               (with and without keyword), -qintsize=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(index('LAlalaA', 'la'));
           if (k1 .ne. 8) error stop 1

           k2 = kind(index('FAAAsaaaa', 'aa', BACK = .FALSE.));
           if (k2 .ne. 8) error stop 2

           k2 = kind(index('PW093Wijsf&(*$A&%', '*$', kind = 4));
           if (k2 .ne. 4) error stop 3

	   k4 = kind(index('JJSud743282', '837', BACK = .TRUE., kind = 4));
           if (k4 .ne. 4) error stop 4

	   k5 = kind(index('+_}{:">#*&$22', '_', .TRUE., 4));
           if (k5 .ne. 4) error stop 5

           r1 = index('P455I747UDsjf2', '747');
           if (r1 .ne. 6) error stop 6

	   r2 = index('#$HiHIHhi', 'HI', BACK = .TRUE.);
           if (r2 .ne. 5) error stop 7

	   r3 = index('4135855', '55', kind = 4);
           if (r3 .ne. 6) error stop 8

	   r4 = index('*Q*Q&^#', 'Q*Q', BACK = .FALSE., kind = 4);
           if (r4 .ne. 2) error stop 9

	   r5 = index('cdefefefe', 'fe', .FALSE., 4);
           if (r5 .ne. 4) error stop 10

END
