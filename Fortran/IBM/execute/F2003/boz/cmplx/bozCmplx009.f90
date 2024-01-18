!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozCmplx009.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 02/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : CMPLX intrinsic
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :Section 13.7.20 - Statement:
!*				CMPLX (X [, Y, KIND])
!*
!*  128 bit pattern for arugements X and Y                         	                             
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

program bozCmplx009
    
    integer(8) :: testI(4)
    complex(16) :: testC

    equivalence(testC, testI)

    testC = &
    CMPLX(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001", &
    Z"42B383C9B0D69ED0", 16)

    if (testI(1) /= &
    B'0100000000100010111000000110011111101101111011101100101001110011') &
    error stop 1_4

    if (testI(2) /= &
    B'0011110010101110000111001011010010101111010101011011101010110001') &
    error stop 2_4

    if (testI(3) /= Z'0') error stop 3_4
    if (testI(4) /= Z'42B383C9B0D69ED0') error stop 4_4

    testC = CMPLX(O"407140357524563001666", &
    B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001", 16) 

    if (testI(1) /= O'0') error stop 5_4
    if (testI(2) /= O"407140357524563001666") error stop 6_4
    if (testI(3) /= &
    B'0100000000100010111000000110011111101101111011101100101001110011') &
    error stop 7_4

    if (testI(4) /= B'0011110010101110000111001011010010101111010101011011101010110001') &
    error stop 8_4

    testC = CMPLX(Z"3fffcd343866873bbc990cb9e52aa19d", &
    O"3776161616161616161621362143434343434343434", 16)

    if (testI(1) /= Z"3fffcd343866873b") error stop 9_4
    if (testI(2) /= Z"bc990cb9e52aa19d") error stop 10_4
    if (testI(3) /= O"1777070707070707070710") error stop 11_4
    if (testI(4) /= O"1362143434343434343434") error stop 12_4


    !X=Lower Bound, Y=Upper Bound
    testC = CMPLX(Z"1", &
    B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111", 16)

    if (testI(1) /= Z"0") error stop 13_4
    if (testI(2) /= Z"1") error stop 14_4
    if (testI(3) /= &
    B"0111111111101111111111111111111111111111111111111111111111111111") &
    error stop 15_4

    if (testI(4) /= &
    B"1111111011111111111111111111111111111111111111111111111111111111") &
    error stop 16_4

    testC = CMPLX(B"1", O"1777377777777777777777773777777777777777777", 16)
    if (testI(1) /= B"0") error stop 17_4
    if (testI(2) /= B"1") error stop 18_4
    if (testI(3) /= O"777577777777777777777") error stop 19_4
    if (testI(4) /= O"1773777777777777777777") error stop 20_4

    testC = CMPLX(O"1", Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF", 16)
    if (testI(1) /= O"0") error stop 21_4
    if (testI(2) /= O"1") error stop 22_4
    if (testI(3) /= Z"7FEFFFFFFFFFFFFF") error stop 23_4
    if (testI(4) /= Z"FEFFFFFFFFFFFFFF") error stop 24_4

    !X=Upper Bound, Y=Lower Bound
    testC = CMPLX(O"1777377777777777777777773777777777777777777", B"1", 16)
    if (testI(1) /= O"777577777777777777777") error stop 25_4
    if (testI(2) /= O"1773777777777777777777") error stop 26_4
    if (testI(3) /= B"0") error stop 27_4
    if (testI(4) /= B"1") error stop 28_4

    testC = CMPLX(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF", O"1", 16)
    if (testI(1) /= Z"7FEFFFFFFFFFFFFF") error stop 29_4
    if (testI(2) /= Z"FEFFFFFFFFFFFFFF") error stop 30_4
    if (testI(3) /= O"0") error stop 31_4
    if (testI(4) /= O"1") error stop 32_4

    testC = &
    CMPLX(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111", &
    Z"1", 16)
    if (testI(1) /= &
    B"0111111111101111111111111111111111111111111111111111111111111111") &
    error stop 33_4

    if (testI(2) /= &
    B"1111111011111111111111111111111111111111111111111111111111111111") &
    error stop 34_4

    if (testI(3) /= Z"0") error stop 35_4
    if (testI(4) /= Z"1") error stop 36_4


    !Test Kind Value
    if (KIND(CMPLX(B"10000000010001011100000011001111110110111101110110010100111001111110010101110000111001011010010101111010101011011101010110001", &
    Z"42B383C9B0D69ED0", 16)) /= 16) error stop 37_4

    if (KIND(CMPLX(O"407140357524563001666", &
    B"10000000010001011100000011001111110110111101110110010100111001111110010101110000111001011010010101111010101011011101010110001", &
    16)) /= 16) error stop 38_4

    if (KIND(CMPLX(Z"3fffcd343866873bbc990cb9e52aa19d", &
    O"3776161616161616161621362143434343434343434", 16)) /= 16) &
    error stop 39_4

    if (KIND(CMPLX(Z"1", &
    B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111", &
    16)) /= 16) error stop 40_4

    if (KIND(CMPLX(B"1", O"1777377777777777777777773777777777777777777", &
    16)) /= 16) error stop 41_4

    if (KIND(CMPLX(O"1", Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF", 16)) /= 16) &
    error stop 42_4

    if (KIND(CMPLX(O"1777377777777777777777773777777777777777777", B"1", &
    16)) /= 16) error stop 43_4

    if (KIND(CMPLX(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF", O"1", 16)) /= 16) &
    error stop 44_4

    if (KIND(CMPLX(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111", &
    Z"1", 16)) /= 16) error stop 45_4

end
