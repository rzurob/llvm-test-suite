!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : DBLE intrinsic
!*
!*  DESCRIPTION                : Section 13.7.29: DBLE(A)
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

program bozDble005

    integer(8) :: testI(2)
    real(16) :: testD

    equivalence(testD, testI)


    testD = &
    DBLE(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001")
    if (testI(1) /= &
    B"0100000000100010111000000110011111101101111011101100101001110011") &
    error stop 1_4

    if (testI(2) /= &
    B"0011110010101110000111001011010010101111010101011011101010110001") &
    error stop 2_4

    testD = DBLE(O"777434343434343434345362143434343434343434")
    if (testI(1) /= O'377616161616161616162') error stop 3_4
    if (testI(2) /= O"1362143434343434343434") error stop 4_4

    testD = DBLE(Z"3fffcd343866873bbc990cb9e52aa19d")
    if (testI(1) /= Z'3FFFCD343866873B') error stop 5_4
    if (testI(2) /= Z"BC990CB9E52AA19D") error stop 6_4

    !Upper Bound
    testD = &
    DBLE(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111")

    if (testI(1) /= &
    B'0111111111101111111111111111111111111111111111111111111111111111') &
    error stop 7_4

    if (testI(2) /= &
    B"1111111011111111111111111111111111111111111111111111111111111111") &
    error stop 8_4

    testD = DBLE(O"1777377777777777777777773777777777777777777")
    if (testI(1) /= O'777577777777777777777') error stop 9_4
    if (testI(2) /= O"1773777777777777777777") error stop 10_4

    testD = DBLE(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF")
    if (testI(1) /= Z'7FEFFFFFFFFFFFFF') error stop 11_4
    if (testI(2) /= Z'FEFFFFFFFFFFFFFF') error stop 12_4

    !Lower Bound
    testD = DBLE(B"1")
    if (testI(1) /= B'0') error stop 13_4
    if (testI(2) /= B"1") error stop 14_4

    testD = DBLE(O"1")
    if (testI(1) /= O'0') error stop 15_4
    if (testI(2) /= O"1") error stop 16_4

    testD = DBLE(Z"1")
    if (testI(1) /= Z'0') error stop 17_4
    if (testI(2) /= Z"1") error stop 18_4

    !Test Kind Value
    if (KIND(DBLE(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001")) &
    /= 16) error stop 19_4

    if (KIND(DBLE(O"777434343434343434345362143434343434343434")) &
    /= 16) error stop 20_4

    if (KIND(DBLE(Z"3FFFcd343866873bbc990cb9e52aa19d")) /= 16) &
    error stop 21_4


    if (KIND(DBLE(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111")) &
    /= 16) error stop 22_4

    if (KIND(DBLE(O"1777377777777777777777773777777777777777777")) &
    /= 16) error stop 23_4

    if (KIND(DBLE(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF")) /= 16) &
    error stop 24_4


    if (KIND(DBLE(B"1")) /= 16) error stop 25_4
    if (KIND(DBLE(O"1")) /= 16) error stop 26_4
    if (KIND(DBLE(Z"1")) /= 16) error stop 27_4

end



