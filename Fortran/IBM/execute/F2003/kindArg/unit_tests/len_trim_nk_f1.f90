!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : len_trim_nk_f1.f
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
!*  DESCRIPTION                : test len_trim functionality without kind
!*                               specified, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890!

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (256) abc
           abc = 'ZKRIerue    '

           k1 = kind(len_trim(abc));
           if (k1 .ne. 4) error stop 1

	   r1 = len_trim(abc);
           if (r1 .ne. 8) error stop 2


END
