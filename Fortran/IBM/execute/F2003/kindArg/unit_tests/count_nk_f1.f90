!*  ===================================================================
!*
!*  TEST CASE NAME             : count_nk_f1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test count functionality without kind
!*                               specified, with various combos of opt
!*                               args, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, r1, r2
           logical(1) :: array(5) = (/.FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)

	   k1 = kind(count(array));
           if (k1 .ne. 4) error stop 1

           k2 = kind(count((/.TRUE., .FALSE., .FALSE./), 1));
           if (k2 .ne. 4) error stop 2

	   r1 = count(array);
           if (r1 .ne. 2) error stop 3

	   r2 = count((/.FALSE., .FALSE., .TRUE./), dim = 1);
           if (r2 .ne. 1) error stop 4

END
