!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozDble006.f
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
!*  DESCRIPTION                :Section 13.7.29: DBLE(A)
!*			The result has the value REAL (A, KIND (0.0D0))
!*                      Passing INF, NaNS, and NaNQ bit pattern   
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

program bozDble006

    real(8) :: testR
    integer(8) :: testI

    equivalence(testR, testI)

    !Positive Infinity
    testR = DBLE(B"1111111100000000000000000000000")
    if (testI /= B"1111111100000000000000000000000") error stop 1_4
    testR = DBLE(O'17740000000')
    if (testI /= O'17740000000') error stop 2_4
    testR = DBLE(Z'7F800000')
    if (testI /= Z'7F800000') error stop 3_4

    !Negative Infinity
    testR = DBLE(b'11111111100000000000000000000000')
    if (testI /= b'11111111100000000000000000000000') error stop 4_4 
    testR = DBLE(O'37740000000')
    if (testI /= O'37740000000') error stop 5_4
    testR = DBLE(Z'FF800000')
    if (testI /= Z'FF800000') error stop 6_4


    !Positive NaNS (Lower Bound)
    testR = DBLE(B'1111111100000000000000000000001')
    if (testI /= B'1111111100000000000000000000001') error stop 7_4  
    testR = DBLE(O'17740000001')
    if (testI /= O'17740000001') error stop 8_4
    testR = DBLE(Z'7F800001')
    if (testI /= Z'7F800001') error stop 9_4

    !Positive NaNS (Upper Bound)
    testR = DBLE(b"1111111101111111111111111111111")
    if (testI /= b"1111111101111111111111111111111") error stop 10_4
    testR = DBLE(O'17757777777')
    if (testI /= O'17757777777') error stop 11_4
    testR = DBLE(Z'7FBFFFFF')
    if (testI /= Z'7FBFFFFF') error stop 12_4


    !Negative NaNS (Lower Bound)
    testR = DBLE(B"11111111100000000000000000000001")
    if (testI /= b'11111111100000000000000000000001') error stop 13_4
    testR = DBLE(O'37740000001')
    if (testI /= O'37740000001') error stop 14_4
    testR = DBLE(Z'FF800001')
    if (testI /= Z'FF800001') error stop 15_4

    !Negative NaNS (Upper Bound)
    testR = DBLE(b'11111111101111111111111111111111')
    if (testI /= B"11111111101111111111111111111111") error stop 16_4
    testR = DBLE(O'37757777777')
    if (testI /= O'37757777777') error stop 17_4
    testR = DBLE(Z'FFBFFFFF')
    if (testI /= Z'FFBFFFFF') error stop 18_4


    !Positive NaNQ (Lower Bound)
    testR = DBLE(B'1111111110000000000000000000000')
    if (testI /= B'1111111110000000000000000000000') error stop 19_4  
    testR = DBLE(O'17760000000')
    if (testI /= O'17760000000') error stop 20_4
    testR = DBLE(Z'7FC00000')
    if (testI /= Z'7FC00000') error stop 21_4

    !Positive NaNQ (Upper Bound)
    testR = DBLE(b"1111111111111111111111111111111")
    if (testI /= b"1111111111111111111111111111111") error stop 22_4
    testR = DBLE(O'17777777777')
    if (testI /= O'17777777777') error stop 23_4
    testR = DBLE(Z'7FFFFFFF')
    if (testI /= Z'7FFFFFFF') error stop 24_4


    !Negative NaNQ (Lower Bound)
    testR = DBLE(B"11111111110000000000000000000000")
    if (testI /= b'11111111110000000000000000000000') error stop 25_4
    testR = DBLE(O'37760000000')
    if (testI /= O'37760000000') error stop 26_4
    testR = DBLE(Z'FFC00000')
    if (testI /= Z'FFC00000') error stop 27_4

    !Negative NaNQ (Upper Bound)
    testR = DBLE(b'11111111111111111111111111111111')
    if (testI /= B"11111111111111111111111111111111") error stop 28_4
    testR = DBLE(O'37777777777')
    if (testI /= O'37777777777') error stop 29_4
    testR = DBLE(Z'FFFFFFFF')
    if (testI /= Z'FFFFFFFF') error stop 30_4


    !Test Kind Value
    if (KIND(DBLE(B"1111111100000000000000000000000")) /= 8) error stop 31_4
    if (KIND(DBLE(O"17740000000")) /= 8) error stop 32_4
    if (KIND(DBLE(Z"7F800000")) /= 8) error stop 33_4

    if (KIND(DBLE(B"11111111100000000000000000000000")) /= 8) error stop 34_4
    if (KIND(DBLE(O"37740000000")) /= 8) error stop 35_4
    if (KIND(DBLE(Z"FF800000")) /= 8) error stop 36_4

    if (KIND(DBLE(B"1111111100000000000000000000001")) /= 8) error stop 37_4
    if (KIND(DBLE(O"17740000001")) /= 8) error stop 38_4
    if (KIND(DBLE(Z"7F800001")) /= 8) error stop 39_4

    if (KIND(DBLE(B"1111111101111111111111111111111")) /= 8) error stop 40_4
    if (KIND(DBLE(O"17757777777")) /= 8) error stop 41_4
    if (KIND(DBLE(Z"7FBFFFFF")) /= 8) error stop 42_4

    if (KIND(DBLE(B"11111111100000000000000000000001")) /= 8) error stop 43_4
    if (KIND(DBLE(O"37740000001")) /= 8) error stop 44_4
    if (KIND(DBLE(Z"FF800001")) /= 8) error stop 45_4

    if (KIND(DBLE(B"11111111101111111111111111111111")) /= 8) error stop 46_4
    if (KIND(DBLE(O"37757777777")) /= 8) error stop 47_4
    if (KIND(DBLE(Z"FFBFFFFF")) /= 8) error stop 48_4

    if (KIND(DBLE(B"1111111110000000000000000000000")) /= 8) error stop 49_4
    if (KIND(DBLE(O"17760000000")) /= 8) error stop 50_4
    if (KIND(DBLE(Z"7FC00000")) /= 8) error stop 51_4

    if (KIND(DBLE(B"1111111111111111111111111111111")) /= 8) error stop 52_4
    if (KIND(DBLE(O"17777777777")) /= 8) error stop 53_4
    if (KIND(DBLE(Z"7FFFFFFF")) /= 8) error stop 54_4

    if (KIND(DBLE(B"11111111110000000000000000000000")) /= 8) error stop 55_4
    if (KIND(DBLE(O"37760000000")) /= 8) error stop 56_4
    if (KIND(DBLE(Z"FFC00000")) /= 8) error stop 57_4

    if (KIND(DBLE(B"11111111111111111111111111111111")) /= 8) error stop 58_4
    if (KIND(DBLE(O"37777777777")) /= 8) error stop 59_4
    if (KIND(DBLE(Z"FFFFFFFF")) /= 8) error stop 60_4

end
