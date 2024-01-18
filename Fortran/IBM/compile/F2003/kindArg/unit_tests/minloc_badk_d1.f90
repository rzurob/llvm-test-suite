!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test error with kind =an invalid value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, r1(1), r2(1)
           integer :: array(2) = (/0, 100/)

	   r1 = minloc(array);
           if (r1(1) .ne. 1) error stop 1

	   k1 = kind(minloc((/2, 6, 4, 1, 5/), kind = r1(1)));

	   r2 = minloc((/12, 443, 23, 1, 52/), kind = 44);
END
