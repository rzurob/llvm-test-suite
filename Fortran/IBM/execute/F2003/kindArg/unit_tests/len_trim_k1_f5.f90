!*  ===================================================================
!*
!*  TEST CASE NAME             : len_trim_k1_f5.f
!*
!*  DATE                       : Apr 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support KIND Argument for new intrinsics
!*
!*  KEYWORD(S)                 : KIND
!*
!*  DESCRIPTION                : test len_trim functionality with various
!*                               kind specified (with and without keyword)
!*                               using simple scalar initialization expressions
!*                               , no -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           integer :: k1, k2, k3, r1, r2, r3
           CHARACTER (256) abc
           abc = '^$&%((GhiJHLM    '

           k1 = kind(len_trim(abc, kind = 1/1*1*1/1));
           if (k1 .ne. 1) error stop 1

	   k2 = kind(len_trim(abc, kind = '1'b));
           if (k2 .ne. 1) error stop 2

	   k3 = kind(len_trim(abc, Z'08'));
           if (k3 .ne. 8) error stop 3

	   r1 = len_trim(abc, kind = 9876*9786*0+2);
           if (r1 .ne. 13) error stop 4

	   r2 = len_trim(abc, kind = '000100'b);
           if (r2 .ne. 13) error stop 5

	   r3 = len_trim(abc, O'0000000001');
           if (r3 .ne. 13) error stop 6

END
