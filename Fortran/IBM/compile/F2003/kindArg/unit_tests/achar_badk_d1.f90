!*  ===================================================================
!*
!*  DATE                       : Apr 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test achar functionality w/ invalid kind
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1
           character*1 :: r1, r2

           r1 = achar(85, 1);
	   if (r1 .ne. 'T') error stop 1

	   k1 = kind(achar(85, kind = r1));

           r2 = achar(85, kind =122);

END
