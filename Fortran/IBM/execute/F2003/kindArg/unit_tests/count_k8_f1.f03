!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test count functionality with various
!*                               combos of opt args, kind = 8 specified
!*                               (with and without keyword), no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5

           k1 = kind(count((/.TRUE., .FALSE., .TRUE./)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(count((/.TRUE., .TRUE./), 1));
           if (k2 .ne. 4) error stop 2

           k3 = kind(count((/ .FALSE., .TRUE./), kind = 8));
           if (k3 .ne. 8) error stop 3

	   k4 = kind(count((/.TRUE., .FALSE., .TRUE./), dim = 1, kind = 8));
           if (k4 .ne. 8) error stop 4

	   k5 = kind(count((/.TRUE., .FALSE., .FALSE./), 1, 8));
           if (k5 .ne. 8) error stop 5

	   r1 = count((/.FALSE., .TRUE., .TRUE./));
           if (r1 .ne. 2) error stop 6

	   r2 = count((/.FALSE., .FALSE., .TRUE./), 1);
           if (r2 .ne. 1) error stop 7

	   r3 = count((/.FALSE., .TRUE., .TRUE./), kind = 8);
           if (r3 .ne. 2) error stop 8

	   r4 = count((/.TRUE., .FALSE., .TRUE., .FALSE./), 1, kind = 8);
           if (r4 .ne. 2) error stop 9

	   r5 = count((/.FALSE., .FALSE., .TRUE./), 1, 8);
           if (r5 .ne. 1) error stop 10

END
