!*  ===================================================================
!*
!*  DATE                       : 02/02/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : CMPLX intrinsic
!*
!*  DESCRIPTION                :Section 13.7.20 - Statement:
!*				CMPLX (X [, Y, KIND])
!*
!*  Testing default Y dummy argument. Default Kind type of 4
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

program bozCmplx002

    complex :: b4, o4, z4


    b4 = CMPLX(10.0, B"111111100111100000011001000010")
    o4 = CMPLX(20.0, O"10432657462")
    z4 = CMPLX(30.0, Z"4A02F53B")

    if (transfer(real(b4 ,4), 0.0_4) /= 10.0) error stop 1_4
    if (transfer(real(o4 ,4), 0.0_4) /= 20.0) error stop 2_4
    if (transfer(real(z4 ,4), 0.0_4) /= 30.0) error stop 3_4

    if (transfer(aimag(b4), 0_4) /= B"111111100111100000011001000010")&
    error stop 4_4

    if (transfer(aimag(o4), 0_4) /= O"10432657462") error stop 5_4
    if (transfer(aimag(z4), 0_4) /= Z"4A02F53B") error stop 6_4


    !Upper Bound
    b4 = CMPLX(40.0, B"1111111011111111111111111111101")
    o4 = CMPLX(50.0, O"17737777775")
    z4 = CMPLX(60.0, Z"7F7FFFFD")

    if (transfer(real(b4 ,4), 0.0_4) /= 40.0) error stop 7_4
    if (transfer(real(o4 ,4), 0.0_4) /= 50.0) error stop 8_4
    if (transfer(real(z4 ,4), 0.0_4) /= 60.0) error stop 9_4

    if (transfer(aimag(b4), 0_4) /= B"1111111011111111111111111111101")&
    error stop 10_4

    if (transfer(aimag(o4), 0_4) /= O"17737777775") error stop 11_4
    if (transfer(aimag(z4), 0_4) /= Z"7F7FFFFD") error stop 12_4


    !Lower Bound
    b4 = CMPLX(70.0, B"1")
    o4 = CMPLX(80.0, O"1")
    z4 = CMPLX(90.0, Z"1")

    if (transfer(real(b4 ,4), 0.0_4) /= 70.0) error stop 13_4
    if (transfer(real(o4 ,4), 0.0_4) /= 80.0) error stop 14_4
    if (transfer(real(z4 ,4), 0.0_4) /= 90.0) error stop 15_4

    if (transfer(aimag(b4), 0_4) /= B"1") error stop 16_4
    if (transfer(aimag(o4), 0_4) /= O"1") error stop 17_4
    if (transfer(aimag(z4), 0_4) /= Z"1") error stop 18_4


    !Test Kind Value
    if (KIND(CMPLX(10.0, B"111111100111100000011001000010")) /= 4)&
    error stop 19_4

    if (KIND(CMPLX(20.0, O"10432657462")) /= 4) error stop 20_4
    if (KIND(CMPLX(30.0, Z"4A02F53B")) /= 4) error stop 21_4

    if (KIND(CMPLX(40.0, B"1111111011111111111111111111101")) /= 4)&
    error stop 22_4

    if (KIND(CMPLX(50.0, O"17737777775")) /= 4) error stop 23_4
    if (KIND(CMPLX(60.0, Z"7F7FFFFD")) /= 4) error stop 24_4

    if (KIND(CMPLX(70.0, B"11111111111111111111100")) /= 4)&
    error stop 25_4

    if (KIND(CMPLX(80.0, O"37777774")) /= 4) error stop 26_4
    if (KIND(CMPLX(90.0, Z"7FFFFC")) /= 4) error stop 27_4

end
