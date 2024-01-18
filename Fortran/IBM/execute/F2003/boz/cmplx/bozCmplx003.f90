!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozCmplx003.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 02/02/2006
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
!*  Testing X as the 2nd argument. Default Kind type of 4                       	                              
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

program bozCmplx003

    complex :: b4, o4, z4


    b4 = CMPLX(Y=1.0, X=B"111111100111100000011001000010")
    o4 = CMPLX(Y=2.0, X=O"10432657462")
    z4 = CMPLX(Y=3.0, X=Z"4A02F53B")

    if (transfer(real(b4 ,4), 0_4) /= B"111111100111100000011001000010")&
    error stop 1_4

    if (transfer(real(o4 ,4), 0_4) /= O"10432657462") error stop 2_4
    if (transfer(real(z4 ,4), 0_4) /= Z"4A02F53B") error stop 3_4

    if (transfer(aimag(b4), 0.0_4) /= 1.0) error stop 4_4
    if (transfer(aimag(o4), 0.0_4) /= 2.0) error stop 5_4
    if (transfer(aimag(z4), 0.0_4) /= 3.0) error stop 6_4


    !Upper Bound
    b4 = CMPLX(Y=4.0, X=B"1111111011111111111111111111101")
    o4 = CMPLX(Y=5.0, X=O"17737777775")
    z4 = CMPLX(Y=6.0, X=Z"7F7FFFFD")

    if (transfer(real(b4 ,4), 0_4) /= B"1111111011111111111111111111101")&
    error stop 7_4

    if (transfer(real(o4 ,4), 0_4) /= O"17737777775") error stop 8_4
    if (transfer(real(z4 ,4), 0_4) /= Z"7F7FFFFD") error stop 9_4

    if (transfer(aimag(b4), 0.0_4) /= 4.0) error stop 10_4
    if (transfer(aimag(o4), 0.0_4) /= 5.0) error stop 11_4
    if (transfer(aimag(z4), 0.0_4) /= 6.0) error stop 12_4


    !Lower Bound
    b4 = CMPLX(Y=7.0, X=B"1")
    o4 = CMPLX(Y=8.0, X=O"1")
    z4 = CMPLX(Y=9.0, X=Z"1")

    if (transfer(real(b4 ,4), 0_4) /= B"1") error stop 13_4
    if (transfer(real(o4 ,4), 0_4) /= O"1") error stop 14_4
    if (transfer(real(z4 ,4), 0_4) /= Z"1") error stop 15_4

    if (transfer(aimag(b4), 0.0_4) /= 7.0) error stop 16_4
    if (transfer(aimag(o4), 0.0_4) /= 8.0) error stop 17_4
    if (transfer(aimag(z4), 0.0_4) /= 9.0) error stop 18_4


    !Test Kind Value
    if (KIND(CMPLX(Y=1.0, X=B"111111100111100000011001000010")) /= 4)&
    error stop 19_4

    if (KIND(CMPLX(Y=2.0, X=O"10432657462")) /= 4) error stop 20_4
    if (KIND(CMPLX(Y=3.0, X=Z"4A02F53B")) /= 4) error stop 21_4

    if (KIND(CMPLX(Y=4.0, X=B"1111111011111111111111111111101")) /= 4)&
    error stop 22_4

    if (KIND(CMPLX(Y=5.0, X=O"17737777775")) /= 4) error stop 23_4
    if (KIND(CMPLX(Y=6.0, X=Z"7F7FFFFD")) /= 4) error stop 24_4

    if (KIND(CMPLX(Y=7.0, X=B"11111111111111111111100")) /= 4)&
    error stop 25_4

    if (KIND(CMPLX(Y=8.0, X=O"37777774")) /= 4) error stop 26_4
    if (KIND(CMPLX(Y=9.0, X=Z"7FFFFC")) /= 4) error stop 27_4

end
