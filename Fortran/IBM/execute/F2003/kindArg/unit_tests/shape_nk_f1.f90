!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : shape_nk_f1.f
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
!*  DESCRIPTION                : test shape functionality without kind
!*                               specified, no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, r1 (1)
           REAL COW (2:9)

           k1 = kind(shape(COW));
           if (k1 .ne. 4) error stop 1

	   r1 = shape(COW);
           if (r1 (1) .ne. 8) error stop 2


END
