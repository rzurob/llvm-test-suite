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

	   r1 = index('BadaDaB', 'ada', kind = 8);
           if (r1 .ne. 2) error stop 1

	   k1 = kind(index('KakAkakakAKKA', 'ka', BACK = .TRUE., kind = r1));

	   r2 = index('|[{:"?>#$&A#', 'A', .FALSE., 99);
END