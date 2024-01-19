!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test scan functionality with various
!*                               combos of opt args, various kinds specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3

           k1 = kind(scan('HiHIHI', 'HI', kind = 2*2/1));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(scan('32894Pans', '32', BACK = .FALSE., kind = '1000'b));
           if (k2 .ne. 8) error stop 2

	   k3 = kind(scan('NarNGor', 'or', .FALSE., O'1'));
           if (k3 .ne. 1) error stop 3

	   r1 = scan('YEjdSK7922', 'SK', kind = 1234-1230+(4*1));
           if (r1 .ne. 5) error stop 4

	   r2 = scan('MKS33HDwqu2', 'HD', BACK = .FALSE., kind = '0001'b);
           if (r2 .ne. 6) error stop 5

	   r3 = scan('72^*$#(4a[s;274;l;fs', '^*', .TRUE., O'10');
           if (r3 .ne. 4) error stop 6

END
