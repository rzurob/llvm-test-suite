!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozDble003.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 02/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : DBLE intrinsic
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Section 13.7.29: DBLE(A)
!*			The result has the value REAL (A, KIND (0.0D0)) 
!*                          	
!*				
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program bozDble003

    if (transfer(DBLE(B"11111111110011110000001100100001110101101111000110111001101001"), &
    0_4) /= transfer(REAL(B"11111111110011110000001100100001110101101111000110111001101001",KIND(0.0D0)), &
    0_4)) error stop 1_4

    if (transfer(DBLE(O"407140357524563001666"), &
    0_4) /= transfer(REAL(O"407140357524563001666",KIND(0.0D0)), 0_4)) &
    error stop 2_4

    if (transfer(DBLE(Z"42B383C9B0D69ED0"), 0_4) &
    /= transfer(REAL(Z"42B383C9B0D69ED0",KIND(0.0D0)), 0_4)) &
    error stop 3_4

    !Upper Bound
    if (transfer(DBLE(B"111111111101111111111111111111111111111111111111111111111111111"), &
    0_4) /= transfer(REAL(B"111111111101111111111111111111111111111111111111111111111111111",KIND(0.0D0)), &
    0_4)) error stop 1_4

    if (transfer(DBLE(O"777577777777777777777"), 0_4) &
    /= transfer(REAL(O"777577777777777777777",KIND(0.0D0)), &
    0_4)) error stop 2_4

    if (transfer(DBLE(Z"7FEFFFFFFFFFFFFF"), 0_4) /= &
    transfer(REAL(Z"7FEFFFFFFFFFFFFF",KIND(0.0D0)), 0_4)) &
    error stop 3_4

    !Lower Bound
    if (transfer(DBLE(B"1"), 0_4) /= &
    transfer(REAL(B"1",KIND(0.0D0)), 0_4)) error stop 1_4

    if (transfer(DBLE(O"1"), 0_4) /= &
    transfer(REAL(O"1",KIND(0.0D0)), 0_4)) error stop 2_4

    if (transfer(DBLE(Z"1"), 0_4) /= &
    transfer(REAL(Z"1",KIND(0.0D0)), 0_4)) error stop 3_4

    !Test Kind Value
    if (KIND(DBLE(B"11111111110011110000001100100001110101101111000110111001101001")) &
    /= 16) error stop 10_4

    if (KIND(REAL(B"11111111110011110000001100100001110101101111000110111001101001",KIND(0.0D0))) &
    /= 16) error stop 11_4

    if (KIND(DBLE(O"407140357524563001666")) /= 16) error stop 12_4
    if (KIND(REAL(O"407140357524563001666",KIND(0.0D0))) /= 16) &
    error stop 13_4

    if (KIND(DBLE(Z"42B383C9B0D69ED0")) /= 16) error stop 14_4
    if (KIND(REAL(Z"42B383C9B0D69ED0",KIND(0.0D0))) /= 16) error stop 15_4


    if (KIND(DBLE(B"111111111101111111111111111111111111111111111111111111111111111")) &
    /= 16) error stop 16_4

    if (KIND(REAL(B"111111111101111111111111111111111111111111111111111111111111111", &
    KIND(0.0D0))) /= 16) error stop 17_4

    if (KIND(DBLE(O"777577777777777777777")) /= 16) error stop 18_4
    if (KIND(REAL(O"777577777777777777777",KIND(0.0D0))) /= 16) &
    error stop 19_4

    if (KIND(DBLE(Z"7FEFFFFFFFFFFFFF")) /= 16) error stop 20_4
    if (KIND(REAL(Z"7FEFFFFFFFFFFFFF",KIND(0.0D0))) /= 16) error stop 21_4


    if (KIND(DBLE(B"1")) /= 16) error stop 22_4
    if (KIND(REAL(B"1",KIND(0.0D0))) /= 16) error stop 23_4

    if (KIND(DBLE(O"1")) /= 16) error stop 24_4
    if (KIND(REAL(O"1",KIND(0.0D0))) /= 16) error stop 25_4

    if (KIND(DBLE(Z"1")) /= 16) error stop 26_4
    if (KIND(REAL(Z"1",KIND(0.0D0))) /= 16) error stop 27_4

end
