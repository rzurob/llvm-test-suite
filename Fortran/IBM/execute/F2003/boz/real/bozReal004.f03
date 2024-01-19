!*  ===================================================================
!*
!*  DATE                       : 02/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : REAL intrinsic
!*
!*  DESCRIPTION                :Parameter A in the REAL intrinsic
!*                              is a boz-literal-constant and KIND
!*                          	is not present, the kind type parameter
!*				is that of default real type.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program bozReal004


    if (transfer(REAL(B"11111111110011110000001100100001110101101111000110111001101001"), &
    0_8) /= B"11111111110011110000001100100001110101101111000110111001101001") &
    error stop 1_4

    if (transfer(REAL(O"407140357524563001666"), 0_8) /= &
    O"407140357524563001666") error stop 2_4

    if (transfer(REAL(Z"42B383C9B0D69ED0"), 0_8) /= &
    Z"42B383C9B0D69ED0") error stop 3_4

    !Upper Bound
    if (transfer(REAL(B"111111111101111111111111111111111111111111111111111111111111111"), &
    0_8) /= B"111111111101111111111111111111111111111111111111111111111111111") &
    error stop 4_4

    if (transfer(REAL(O"777577777777777777777"), 0_8) /= &
    O"777577777777777777777") error stop 5_4

    if (transfer(REAL(Z"7FEFFFFFFFFFFFFF"), 0_8) /= &
    Z"7FEFFFFFFFFFFFFF") error stop 6_4

    !Lower Bound
    if (transfer(REAL(B"1"), 0_8) /= B"1") error stop 7_4
    if (transfer(REAL(O"1"), 0_8) /= O"1") error stop 8_4
    if (transfer(REAL(Z"1"), 0_8) /= Z"1") error stop 9_4

    !Test Kind Value
    if (KIND(REAL(B"111111100111100000011001000010")) /= 8) &
    error stop 10_4

    if (KIND(REAL(O"10432657462")) /= 8) error stop 11_4
    if (KIND(REAL(Z"4A02F53B")) /= 8) error stop 12_4


    if (KIND(REAL(B"1111111011111111111111111111101")) /= 8) &
    error stop 13_4

    if (KIND(REAL(O"17737777775")) /= 8) error stop 14_4
    if (KIND(REAL(Z"7F7FFFFD")) /= 8) error stop 15_4


    if (KIND(REAL(B"11111111111111111111100")) /= 8) error stop 16_4
    if (KIND(REAL(O"37777774")) /= 8) error stop 17_4
    if (KIND(REAL(Z"7FFFFC")) /= 8) error stop 18_4

end
