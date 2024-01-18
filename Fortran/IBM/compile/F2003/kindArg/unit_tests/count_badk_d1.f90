!*  ===================================================================
!*
!*  TEST CASE NAME             : count_badk_d1.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test error with kind =an invalid value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, r1, r2
           logical(1) :: array(4) = (/.FALSE., .TRUE., .FALSE., .TRUE./)

	   r1 = count((/.TRUE., .TRUE., .TRUE./));
           if (r1 .ne. 3) error stop 1

	   k1 = kind(count(array, kind = r1));

	   r2 = count((/.TRUE., .TRUE., .TRUE./), kind = 9);
END
