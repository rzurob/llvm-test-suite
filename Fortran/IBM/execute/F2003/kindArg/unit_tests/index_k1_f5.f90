!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : index_k1_f5.f
!*
!*  PROGRAMMER                 : Vince Yuen
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test index functionality with various 
!*                               combos of opt args, various kind specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3

           k1 = kind(index('123742', '74', kind = 938-937));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(index('abcK34ISHha', 'K34', BACK = .FALSE., kind = '0100'b));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(index('LOs0d9#', 'KL', .FALSE., O'10'));
           if (k3 .ne. 8) error stop 3

	   r1 = index('Ia];jHFs943726', 'jHFs', kind = 8*8/8-4);
           if (r1 .ne. 5) error stop 4

	   r2 = index('092|][[s3hJSD]2', '[[', BACK = .FALSE., kind = '0001'b);
           if (r2 .ne. 6) error stop 5

	   r3 = index('kis9d732', 'r', .TRUE., O'10');
           if (r3 .ne. 0) error stop 6

END
