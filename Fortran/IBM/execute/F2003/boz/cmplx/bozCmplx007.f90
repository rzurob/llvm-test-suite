!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozCmplx007.f
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
!* Specifying all three arguements in the order of KIND, Y, X. 
!* Specifying kind type as 4                        	
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

program bozCmplx007

    complex :: b4, o4, z4


    b4 = CMPLX(KIND=4, Y=B"111111100111100000011001000010", X=0.0)
    o4 = CMPLX(KIND=4, Y=O"10432657462", X=0.0)
    z4 = CMPLX(KIND=4, Y=Z"4A02F53B", X=0.0)

    if (transfer(real(b4 ,4), 0_4) /= 0.0) error stop 1_4
    if (transfer(real(o4 ,4), 0_4) /= 0.0) error stop 2_4
    if (transfer(real(z4 ,4), 0_4) /= 0.0) error stop 3_4

    if (transfer(aimag(b4), 0_4) /= B"111111100111100000011001000010") &
    error stop 4_4

    if (transfer(aimag(o4), 0_4) /= O"10432657462") error stop 5_4
    if (transfer(aimag(z4), 0_4) /= Z"4A02F53B") error stop 6_4


    !X=Lower Bound, Y=Upper Bound
    b4 = CMPLX(KIND=4, Y=B"1111111011111111111111111111101", X=Z"1")
    o4 = CMPLX(KIND=4, Y=O"17737777775", X=B"1")
    z4 = CMPLX(KIND=4, Y=Z"7F7FFFFD", X=O"1")

    if (transfer(real(b4 ,4), 0_4) /= Z"1") error stop 7_4
    if (transfer(real(o4 ,4), 0_4) /= B"1") error stop 8_4
    if (transfer(real(z4 ,4), 0_4) /= O"1") error stop 9_4

    if (transfer(aimag(b4), 0_4) /= B"1111111011111111111111111111101") &
    error stop 10_4

    if (transfer(aimag(o4), 0_4) /= O"17737777775") error stop 11_4
    if (transfer(aimag(z4), 0_4) /= Z"7F7FFFFD") error stop 12_4


    !X=Upper Bound, Y=Lower Bound
    b4 = CMPLX(KIND=4, Y=B"1", X=O"17737777775")
    o4 = CMPLX(KIND=4, Y=O"1", X=Z"7F7FFFFD")
    z4 = CMPLX(KIND=4, Y=Z"1", X=B"1111111011111111111111111111101")

    if (transfer(real(b4 ,4), 0_4) /= O"17737777775") error stop 13_4
    if (transfer(real(o4 ,4), 0_4) /= Z"7F7FFFFD") error stop 14_4
    if (transfer(real(z4 ,4), 0_4) /= B"1111111011111111111111111111101") &
    error stop 15_4

    if (transfer(aimag(b4), 0_4) /= B"1") error stop 16_4
    if (transfer(aimag(o4), 0_4) /= O"1") error stop 17_4
    if (transfer(aimag(z4), 0_4) /= Z"1") error stop 18_4


    !Test Kind Value
    if (KIND(CMPLX(KIND=4, Y=B"111111100111100000011001000010", X=0.0)) /= 4) &
    error stop 19_4

    if (KIND(CMPLX(KIND=4, Y=O"10432657462", X=0.0)) /= 4) error stop 20_4
    if (KIND(CMPLX(KIND=4, Y=Z"4A02F53B", X=0.0)) /= 4) error stop 21_4

    if (KIND(CMPLX(KIND=4, Y=B"1111111011111111111111111111101", X=Z"1")) /= 4) &
    error stop 22_4

    if (KIND(CMPLX(KIND=4, Y=O"17737777775", X=B"1")) /= 4) error stop 23_4
    if (KIND(CMPLX(KIND=4, Y=Z"7F7FFFFD", X=O"1")) /= 4) error stop 24_4

    if (KIND(CMPLX(KIND=4, Y=B"1", X=O"17737777775")) /= 4) error stop 25_4
    if (KIND(CMPLX(KIND=4, Y=O"1", X=Z"7F7FFFFD")) /= 4) error stop 26_4
    if (KIND(CMPLX(KIND=4, Y=Z"1", X=B"1111111011111111111111111111101")) /= 4) &
    error stop 27_4

end
