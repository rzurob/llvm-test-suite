!*  ===================================================================
!*
!*  TEST CASE NAME             : count_k1_f5.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test count functionality with various
!*                               combos of opt args, various kind specified
!*                               (with and without keyword) using simple
!*                               scalar initialization expressions, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3

           k1 = kind(count((/.TRUE., .FALSE., .TRUE./), kind = 4+0*8374+(-2)));
           if (k1 .ne. 2) error stop 1

	   k2 = kind(count((/.TRUE., .FALSE., .TRUE./), 1, kind = '0100'b));
           if (k2 .ne. 4) error stop 2

	   k3 = kind(count((/.TRUE., .FALSE., .TRUE./), 1, Z'0001'));
           if (k3 .ne. 1) error stop 3

	   r1 = count((/.FALSE., .TRUE., .TRUE./), kind = -99+100*1/1+1-1);
           if (r1 .ne. 2) error stop 4

	   r2 = count((/.FALSE., .FALSE., .TRUE., .FALSE./), 1, kind = '0001'b);
           if (r2 .ne. 1) error stop 5

	   r3 = count((/.FALSE., .TRUE., .TRUE./), 1, O'10');
           if (r3 .ne. 2) error stop 6

END
