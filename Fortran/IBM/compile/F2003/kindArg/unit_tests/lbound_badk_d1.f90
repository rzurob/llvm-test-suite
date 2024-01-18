!*  ===================================================================
!*
!*  TEST CASE NAME             : lbound_badk_d1.f
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
           REAL A (2:5, -1:25)

	   r1 = lbound(A, 1);
           if (r1 .ne. 2) error stop 1

	   k1 = kind(lbound(A, kind = r1));

	   r2 = lbound(A, kind = -1843);
END
