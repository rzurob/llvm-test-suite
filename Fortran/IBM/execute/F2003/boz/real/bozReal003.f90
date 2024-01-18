!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozReal003.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 02/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : REAL intrinsic
!*
!*  DRIVER STANZA              : xlf2003
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

program bozReal003

    if (transfer(REAL(B"111111100111100000011001000010"), 0_4) /= &
    B"111111100111100000011001000010") error stop 1_4

    if (transfer(REAL(O"10432657462"), 0_4) /= O"10432657462") &
    error stop 2_4

    if (transfer(REAL(Z"4A02F53B"), 0_4) /= Z"4A02F53B") error stop 3_4

    !Upper Bound
    if (transfer(REAL(B"1111111011111111111111111111101"), 0_4) /= &
    B"1111111011111111111111111111101") error stop 1_4

    if (transfer(REAL(O"17737777775"), 0_4) /= O"17737777775") &
    error stop 2_4

    if (transfer(REAL(Z"7F7FFFFD"), 0_4) /= Z"7F7FFFFD") error stop 3_4

    !Lower Bound
    if (transfer(REAL(B"11111111111111111111100"), 0_4) /= &
    B"11111111111111111111100") error stop 1_4

    if (transfer(REAL(O"37777774"), 0_4) /= O"37777774") error stop 2_4
    if (transfer(REAL(Z"7FFFFC"), 0_4) /= Z"7FFFFC") error stop 3_4

    !Test Kind Value
    if (KIND(REAL(B"111111100111100000011001000010", 4)) /= 4) &
    error stop 10_4

    if (KIND(REAL(O"10432657462", 4)) /= 4) error stop 11_4
    if (KIND(REAL(Z"4A02F53B", 4)) /= 4) error stop 12_4

    if (KIND(REAL(B"1111111011111111111111111111101", 4)) /= 4) &
    error stop 13_4

    if (KIND(REAL(O"17737777775", 4)) /= 4) error stop 14_4
    if (KIND(REAL(Z"7F7FFFFD", 4)) /= 4) error stop 15_4

    if (KIND(REAL(B"11111111111111111111100", 4)) /= 4) &
    error stop 16_4

    if (KIND(REAL(O"37777774", 4)) /= 4) error stop 17_4
    if (KIND(REAL(Z"7FFFFC", 4)) /= 4) error stop 18_4

end
