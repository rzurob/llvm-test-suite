!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : REAL intrinsic
!*
!*  DESCRIPTION                :If A is a boz-literal-constant
!*                              and KIND is present, the kind
!*                          	type parameter is that specified
!*				by the value of KIND
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program bozReal007

    real(16) :: testR
    integer(8) :: testI(2)

    equivalence(testR, testI)

    testR = &
    REAL(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001", 16)

    if (testI(1) /= &
    B"0100000000100010111000000110011111101101111011101100101001110011") &
    error stop 1_4

    if (testI(2) /= &
    B"0011110010101110000111001011010010101111010101011011101010110001") &
    error stop 2_4

    testR = REAL(O"777434343434343434345362143434343434343434", 16)
    if (testI(1) /= O'377616161616161616162') error stop 3_4
    if (testI(2) /= O"1362143434343434343434") error stop 4_4

    testR = REAL(Z"3fffcd343866873bbc990cb9e52aa19d", 16)
    if (testI(1) /= Z'3FFFCD343866873B') error stop 5_4
    if (testI(2) /= Z"BC990CB9E52AA19D") error stop 6_4

    !Upper Bound
    testR = &
    REAL(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111", 16)

    if (testI(1) /= &
    B'0111111111101111111111111111111111111111111111111111111111111111') &
    error stop 7_4

    if (testI(2) /= &
    B"1111111011111111111111111111111111111111111111111111111111111111") &
    error stop 8_4

    testR = REAL(O"1777377777777777777777773777777777777777777", 16)
    if (testI(1) /= O'777577777777777777777') error stop 9_4
    if (testI(2) /= O"1773777777777777777777") error stop 10_4

    testR = REAL(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF", 16)
    if (testI(1) /= Z'7FEFFFFFFFFFFFFF') error stop 11_4
    if (testI(2) /= Z'FEFFFFFFFFFFFFFF') error stop 12_4

    !Lower Bound
    testR = REAL(B"1", 16)
    if (testI(1) /= B'0') error stop 13_4
    if (testI(2) /= B"1") error stop 14_4

    testR = REAL(O"1", 16)
    if (testI(1) /= O'0') error stop 15_4
    if (testI(2) /= O"1") error stop 16_4

    testR = REAL(Z"1", 16)
    if (testI(1) /= Z'0') error stop 17_4
    if (testI(2) /= Z"1") error stop 18_4

    !Test Kind Value
    if (KIND(REAL(B"11111111110011110000001100100001110101101111000110111001101001", &
    16)) /= 16) error stop 19_4

    if (KIND(REAL(O"407140357524563001666", 16)) /= 16) error stop 20_4
    if (KIND(REAL(Z"42B383C9B0D69ED0", 16)) /= 16) error stop 21_4

    if (KIND(REAL(B"111111111101111111111111111111111111111111111111111111111111111", &
    16)) /= 16) error stop 22_4

    if (KIND(REAL(O"777577777777777777777", 16)) /= 16) error stop 23_4
    if (KIND(REAL(Z"7FEFFFFFFFFFFFFF", 16)) /= 16) error stop 24_4

    if (KIND(REAL(B"1", 16)) /= 16) error stop 25_4
    if (KIND(REAL(O"1", 16)) /= 16) error stop 26_4
    if (KIND(REAL(Z"1", 16)) /= 16) error stop 27_4

end
