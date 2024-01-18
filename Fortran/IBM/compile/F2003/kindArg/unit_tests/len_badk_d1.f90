!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : len_badk_d1.f
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
!*  DESCRIPTION                : test len with kind =an invalid value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      CHARACTER (129) abc
      abc = 'XYZ'

	   r1 = len(abc);
           if (r1 .ne. 3) error stop 1

	   k1 = kind(len(abc , kind = r1));

	   r2 = len(abc , -11);
END
