!*  ===================================================================
!*
!*  DATE                       : 02/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : CMPLX intrinsic
!*
!*  DESCRIPTION                :Section 13.7.20 - Statement:
!*				CMPLX (X [, Y, KIND])
!*
!* Specifying all three arguements in the order of KIND, Y, X.
!* Specifying kind type as 16
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

program bozCmplx008

    integer(8) :: testI(4)
    complex(16) :: testC

    equivalence(testC, testI)

    testC = CMPLX(KIND=16, Y=B"111111100111100000011001000010", &
    X=Z"42B383C9B0D69ED0")

    if (testI(1) /= Z'0') error stop 1_4
    if (testI(2) /= Z'42B383C9B0D69ED0') error stop 2_4
    if (testI(3) /= B'0') error stop 3_4
    if (testI(4) /= B'111111100111100000011001000010') error stop 4_4

    testC = CMPLX(KIND=16, Y=O"10432657462", &
    X=B"11111111110011110000001100100001110101101111000110111001101001")

    if (testI(1) /= B'0') error stop 5_4
    if (testI(2) /= B'11111111110011110000001100100001110101101111000110111001101001') &
    error stop 6_4

    if (testI(3) /= O'0') error stop 7_4
    if (testI(4) /= O'10432657462') error stop 8_4

    testC = CMPLX(KIND=16, Y=Z"4A02F53B", X=O"407140357524563001666")
    if (testI(1) /= O'0') error stop 9_4
    if (testI(2) /= O'407140357524563001666') error stop 10_4
    if (testI(3) /= Z'0') error stop 11_4
    if (testI(4) /= Z'4A02F53B') error stop 12_4

    !X=Lower Bound, Y=Upper Bound
    testC = CMPLX(KIND=16, &
    Y=B"111111111101111111111111111111111111111111111111111111111111111", &
    X=Z"1")

    if (testI(1) /= Z'0') error stop 13_4
    if (testI(2) /= Z'1') error stop 14_4
    if (testI(3) /= B'0') error stop 15_4
    if (testI(4) /= &
    B'111111111101111111111111111111111111111111111111111111111111111') &
    error stop 16_4

    testC = CMPLX(KIND=16, Y=O"777577777777777777777", X=B"1")
    if (testI(1) /= B'0') error stop 17_4
    if (testI(2) /= B'1') error stop 18_4
    if (testI(3) /= O'0') error stop 19_4
    if (testI(4) /= O'777577777777777777777') error stop 20_4

    testC = CMPLX(KIND=16, Y=Z"7FEFFFFFFFFFFFFF", X=O"1")
    if (testI(1) /= O'0') error stop 21_4
    if (testI(2) /= O'1') error stop 22_4
    if (testI(3) /= Z'0') error stop 23_4
    if (testI(4) /= Z'7FEFFFFFFFFFFFFF') error stop 24_4

    !X=Upper Bound, Y=Lower Bound
    testC = CMPLX(KIND=16, Y=B"1", X=O"777577777777777777777")
    if (testI(1) /= O'0') error stop 25_4
    if (testI(2) /= O'777577777777777777777') error stop 26_4
    if (testI(3) /= B'0') error stop 27_4
    if (testI(4) /= B'1') error stop 28_4

    testC = CMPLX(KIND=16, Y=O"1", X=Z"7FEFFFFFFFFFFFFF")
    if (testI(1) /= Z'0') error stop 29_4
    if (testI(2) /= Z'7FEFFFFFFFFFFFFF') error stop 30_4
    if (testI(3) /= O'0') error stop 31_4
    if (testI(4) /= O'1') error stop 32_4

    testC = CMPLX(KIND=16, Y=Z"1", &
    X=B"111111111101111111111111111111111111111111111111111111111111111")

    if (testI(1) /= B'0') error stop 32_4
    if (testI(2) /= &
    B'111111111101111111111111111111111111111111111111111111111111111') &
    error stop 33_4

    if (testI(3) /= Z'0') error stop 34_4
    if (testI(4) /= Z'1') error stop 35_4

    !Test Kind Value
    if (KIND(CMPLX(KIND=16, Y=B"111111100111100000011001000010", &
    X=Z"42B383C9B0D69ED0")) /= 16) error stop 36_4

    if (KIND(CMPLX(KIND=16, Y=O"10432657462", &
    X=B"11111111110011110000001100100001110101101111000110111001101001")) /= 16) &
    error stop 37_4

    if (KIND(CMPLX(KIND=16, Y=Z"4A02F53B", X=O"407140357524563001666")) /= 16) &
    error stop 38_4

    if (KIND(CMPLX(KIND=16, &
    Y=B"111111111101111111111111111111111111111111111111111111111111111", &
    X=Z"1")) /= 16) error stop 39_4

    if (KIND(CMPLX(KIND=16, Y=O"777577777777777777777", X=B"1")) /= 16) &
    error stop 40_4

    if (KIND(CMPLX(KIND=16, Y=Z"7FEFFFFFFFFFFFFF", X=O"1")) /= 16) &
    error stop 41_4

    if (KIND(CMPLX(KIND=16, Y=B"1", X=O"777577777777777777777")) /= 16) &
    error stop 42_4

    if (KIND(CMPLX(KIND=16, Y=O"1", X=Z"7FEFFFFFFFFFFFFF")) /= 16) &
    error stop 43_4

    if (KIND(CMPLX(KIND=16, Y=Z"1", &
    X=B"111111111101111111111111111111111111111111111111111111111111111")) /= 16) &
    error stop 44_4

end
