!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : count_k2_f1.f
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
!*  DESCRIPTION                : test count functionality with various 
!*                               combos of opt args, kind = 2 specified
!*                               (with and without keyword), -qintsize=4
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, k4, k5, r1, r2, r3, r4, r5
           logical(1) :: array(5) = (/.TRUE., .TRUE., .FALSE., .TRUE., .TRUE./) 

           k1 = kind(count((/.TRUE., .FALSE., .TRUE./)));
           if (k1 .ne. 4) error stop 1

	   k2 = kind(count((/.TRUE., .FALSE., .TRUE./), 1));
           if (k2 .ne. 4) error stop 2

           k3 = kind(count((/.TRUE., .FALSE., .TRUE./), kind = 2));
           if (k3 .ne. 2) error stop 3

	   k4 = kind(count(array, dim = 1, kind = 2));
           if (k4 .ne. 2) error stop 4

	   k5 = kind(count((/.TRUE., .FALSE., .TRUE./), 1, 2));
           if (k5 .ne. 2) error stop 5

	   r1 = count((/.TRUE., .TRUE., .TRUE./));
           if (r1 .ne. 3) error stop 6

	   r2 = count(array, 1);
           if (r2 .ne. 4) error stop 7

	   r3 = count((/.FALSE., .TRUE., .TRUE./), kind = 2);
           if (r3 .ne. 2) error stop 8

	   r4 = count((/.TRUE., .FALSE., .TRUE., .FALSE./), dim = 1, kind = 2);
           if (r4 .ne. 2) error stop 9

	   r5 = count((/.FALSE., .FALSE., .TRUE./), 1, 2);
           if (r5 .ne. 1) error stop 10

END
