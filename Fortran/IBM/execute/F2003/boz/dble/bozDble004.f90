!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozDble004.f
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
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program bozDble004

    integer(8) :: testI1(2), testI2(2)
    real(16) :: testD, testR

    equivalence(testD, testI1)
    equivalence(testR, testI2)

    testD = &
    DBLE(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001")

    testR = &
    REAL(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001",KIND(0.0D0))

    if (testI1(1) /= testI2(1)) error stop 1_4
    if (testI1(2) /= testI2(2)) error stop 2_4

    testD = DBLE(O"777434343434343434345362143434343434343434")
    testR = &
    REAL(O"777434343434343434345362143434343434343434",KIND(0.0D0))

    if (testI1(1) /= testI2(1)) error stop 3_4
    if (testI1(2) /= testI2(2)) error stop 4_4

    testD = DBLE(Z"3fffcd343866873bbc990cb9e52aa19d")
    testR = REAL(Z"3fffcd343866873bbc990cb9e52aa19d",KIND(0.0D0))
    if (testI1(1) /= testI2(1)) error stop 5_4
    if (testI1(2) /= testI2(2)) error stop 6_4


    !Upper Bound
    testD = &
    DBLE(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111")

    testR = &
    REAL(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111",KIND(0.0D0))

    if (testI1(1) /= testI2(1)) error stop 7_4
    if (testI1(2) /= testI2(2)) error stop 8_4

    testD = DBLE(O"1777377777777777777777773777777777777777777")
    testR = &
    REAL(O"1777377777777777777777773777777777777777777",KIND(0.0D0))

    if (testI1(1) /= testI2(1)) error stop 9_4
    if (testI1(2) /= testI2(2)) error stop 10_4

    testD = DBLE(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF")
    testR = REAL(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF",KIND(0.0D0))
    if (testI1(1) /= testI2(1)) error stop 11_4
    if (testI1(2) /= testI2(2)) error stop 12_4


    !Lower Bound
    testD = DBLE(B"1")
    testR = REAL(B"1",KIND(0.0D0))
    if (testI1(1) /= testI2(1)) error stop 13_4
    if (testI1(2) /= testI2(2)) error stop 14_4

    testD = DBLE(O"1")
    testR = REAL(O"1",KIND(0.0D0))
    if (testI1(1) /= testI2(1)) error stop 15_4
    if (testI1(2) /= testI2(2)) error stop 16_4

    testD = DBLE(Z"1")
    testR = REAL(Z"1",KIND(0.0D0))
    if (testI1(1) /= testI2(1)) error stop 17_4
    if (testI1(2) /= testI2(2)) error stop 18_4


    !Test Kind Value
    if (KIND(DBLE(B"01000000001000101110000001100111111011011110111011001010011100110011110010101110000111001011010010101111010101011011101010110001")) &
    /= 16) error stop 19_4

    if (KIND(DBLE(O"777434343434343434345362143434343434343434")) /= 16) &
    error stop 20_4

    if (KIND(DBLE(Z"3FFFcd343866873bbc990cb9e52aa19d")) /= 16) error stop 21_4

    if (KIND(DBLE(B"01111111111011111111111111111111111111111111111111111111111111111111111011111111111111111111111111111111111111111111111111111111")) &
    /= 16) error stop 22_4

    if (KIND(DBLE(O"1777377777777777777777773777777777777777777")) /= 16) &
    error stop 23_4

    if (KIND(DBLE(Z"7FeFFFFFFFFFFFFFFeFFFFFFFFFFFFFF")) /= 16) error stop 24_4

    if (KIND(DBLE(B"1")) /= 16) error stop 25_4
    if (KIND(DBLE(O"1")) /= 16) error stop 26_4
    if (KIND(DBLE(Z"1")) /= 16) error stop 27_4

end
