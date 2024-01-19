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

           integer :: k1, r1, r2

	   r1 = scan('KDITple', 'T', kind = 8);
           if (r1 .ne. 4) error stop 1

	   k1 = kind(scan('PO8723jsF', 'P6', BACK = .TRUE., kind = r1));

	   r2 = scan('FORTRAN', 'TR', .FALSE., 99);
END
