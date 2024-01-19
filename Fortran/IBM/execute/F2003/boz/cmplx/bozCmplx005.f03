!*  ===================================================================
!*
!*  DATE                       : 02/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : CMPLX intrinsic
!*
!*  DESCRIPTION                :Section 13.7.20 - Statement:
!*				CMPLX (X [, Y, KIND])
!*
!* Testing default X dummy argument and explicitly defining the
!* Kind argument without specifying the Y argument. Kind type 8
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

program bozCmplx005

    complex(8) :: b8, o8, z8

    b8 = cmplx(B"11111111110011110000001100100001110101101111000110111001101001", kind=8)
    o8 = cmplx(O"407140357524563001666", kind=8)
    z8 = cmplx(Z"42B383C9B0D69ED0", kind=8)

    if (transfer(real(b8,8), 0_8) /= &
    B"11111111110011110000001100100001110101101111000110111001101001") &
    error stop 1_4

    if (transfer(real(o8,8), 0_8) /= O"407140357524563001666") &
    error stop 2_4

    if (transfer(real(z8,8), 0_8) /= Z"42B383C9B0D69ED0") &
    error stop 3_4

    if (transfer(aimag(b8), 0_8) /= 0.0) error stop 4_4
    if (transfer(aimag(o8), 0_8) /= 0.0) error stop 5_4
    if (transfer(aimag(z8), 0_8) /= 0.0) error stop 6_4


    !Upper Bound
    b8 = cmplx(B"111111111101111111111111111111111111111111111111111111111111111", kind=8)
    o8 = cmplx(O"777577777777777777777", kind=8)
    z8 = cmplx(Z"7FEFFFFFFFFFFFFF", kind=8)

    if (transfer(real(b8,8), 0_8) /= &
    B"111111111101111111111111111111111111111111111111111111111111111") &
    error stop 7_4

    if (transfer(real(o8,8), 0_8) /= O"777577777777777777777") &
    error stop 8_4

    if (transfer(real(z8,8), 0_8) /= Z"7FEFFFFFFFFFFFFF") &
    error stop 9_4

    if (transfer(aimag(b8), 0_8) /= 0.0) error stop 10_4
    if (transfer(aimag(o8), 0_8) /= 0.0) error stop 11_4
    if (transfer(aimag(z8), 0_8) /= 0.0) error stop 12_4


    !Lower Bound
    b8 = cmplx(B"1", kind=8)
    o8 = cmplx(O"1", kind=8)
    z8 = cmplx(Z"1", kind=8)

    if (transfer(real(b8,8), 0_8) /= B"1") error stop 13_4
    if (transfer(real(o8,8), 0_8) /= O"1") error stop 14_4
    if (transfer(real(z8,8), 0_8) /= Z"1") error stop 15_4

    if (transfer(aimag(b8), 0_8) /= 0.0) error stop 16_4
    if (transfer(aimag(o8), 0_8) /= 0.0) error stop 17_4
    if (transfer(aimag(z8), 0_8) /= 0.0) error stop 18_4


    !Test Kind Value
    if (KIND(CMPLX(B"11111111110011110000001100100001110101101111000110111001101001", KIND=8)) /= 8) &
    error stop 19_4

    if (KIND(CMPLX(O"407140357524563001666", KIND=8)) /= 8) error stop 20_4
    if (KIND(CMPLX(Z"42B383C9B0D69ED0", KIND=8)) /= 8) error stop 21_4

    if (KIND(CMPLX(B"111111111101111111111111111111111111111111111111111111111111111", KIND=8)) /= 8) &
    error stop 22_4

    if (KIND(CMPLX(O"777577777777777777777", KIND=8)) /= 8) error stop 23_4
    if (KIND(CMPLX(Z"7FEFFFFFFFFFFFFF", KIND=8)) /= 8) error stop 24_4

    if (KIND(CMPLX(B"1", KIND=8)) /= 8) error stop 25_4
    if (KIND(CMPLX(O"1", KIND=8)) /= 8) error stop 26_4
    if (KIND(CMPLX(Z"1", KIND=8)) /= 8) error stop 27_4

  end
