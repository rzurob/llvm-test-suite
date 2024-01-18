!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : DBLE intrinsic
!*
!*  DESCRIPTION                :Section 13.7.29: DBLE(A)
!*			The result has the value REAL (A, KIND (0.0D0))
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

program bozDble001

    if (transfer(DBLE(B"11111111110011110000001100100001110101101111000110111001101001"), &
    0_8) /= &
    B"11111111110011110000001100100001110101101111000110111001101001") &
    error stop 1_4

    if (transfer(DBLE(O"407140357524563001666"), 0_8) /= &
    O"407140357524563001666") error stop 2_4

    if (transfer(DBLE(Z"42B383C9B0D69ED0"), 0_8) /= Z"42B383C9B0D69ED0") &
    error stop 3_4

    !Upper Bound
    if (transfer(DBLE(B"111111111101111111111111111111111111111111111111111111111111111"), &
    0_8) /= B"111111111101111111111111111111111111111111111111111111111111111") &
    error stop 4_4

    if (transfer(DBLE(O"777577777777777777777"), 0_8) /= O"777577777777777777777") &
    error stop 5_4

    if (transfer(DBLE(Z"7FEFFFFFFFFFFFFF"), 0_8) /= Z"7FEFFFFFFFFFFFFF") &
    error stop 6_4

    !Lower Bound
    if (transfer(DBLE(B"1"), 0_8) /= B"1") error stop 7_4
    if (transfer(DBLE(O"1"), 0_8) /= O"1") error stop 8_4
    if (transfer(DBLE(Z"1"), 0_8) /= Z"1") error stop 9_4

    !Test Kind Value
    if (KIND(DBLE(B"11111111110011110000001100100001110101101111000110111001101001")) &
    /= 8) error stop 10_4

    if (KIND(DBLE(O"407140357524563001666")) /= 8) error stop 11_4
    if (KIND(DBLE(Z"42B383C9B0D69ED0")) /= 8) error stop 12_4


    if (KIND(DBLE(B"111111111101111111111111111111111111111111111111111111111111111")) &
    /= 8) error stop 13_4
    if (KIND(DBLE(O"777577777777777777777")) /= 8) error stop 14_4
    if (KIND(DBLE(Z"7FEFFFFFFFFFFFFF")) /= 8) error stop 15_4


    if (KIND(DBLE(B"1")) /= 8) error stop 16_4
    if (KIND(DBLE(O"1")) /= 8) error stop 17_4
    if (KIND(DBLE(Z"1")) /= 8) error stop 18_4


end
