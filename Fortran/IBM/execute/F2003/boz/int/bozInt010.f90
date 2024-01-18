!*  ===================================================================
!*
!*  DATE                       : 01/23/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : INT intrinsic using KIND parameter
!*
!*  DESCRIPTION                : A boz-literal-constant shall appear
!*				 as the actual argument associated with
!*				the dummy argument A of the numeric
!*				intrinsic function INT. If KIND is
!*				present, the kind type parameter is
!*				that specified by the value of KIND.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program bozInt010

      integer b4, o4, z4

      ! Four Bytes
      if (INT(B'1011110001011001101010011111011') /= 1579996411) &
      error stop 1_4

      if (INT(O'13613152373') /= 1579996411) error stop 2_4
      if (INT(Z'5E2CD4FB') /= 1579996411) error stop 3_4
      if (INT(B'1011110001011001101010011111011') /= &
      B'1011110001011001101010011111011') error stop 1_4

      if (INT(O'13613152373') /= O'13613152373') error stop 2_4
      if (INT(Z'5E2CD4FB') /= Z'5E2CD4FB') error stop 3_4

      b4 = INT(B'1011110001011001101010011111011')
      o4 = INT(O'13613152373')
      z4 = INT(Z'5E2CD4FB')

      if (b4 /= 1579996411) error stop 7_4
      if (o4 /= 1579996411) error stop 8_4
      if (z4 /= 1579996411) error stop 9_4
      if (b4 /= B'1011110001011001101010011111011') error stop 7_4
      if (o4 /= O'13613152373') error stop 8_4
      if (z4 /= Z'5E2CD4FB') error stop 9_4

      if (KIND(b4) /= 4) error stop 10_4
      if (KIND(o4) /= 4) error stop 11_4
      if (KIND(z4) /= 4) error stop 12_4

      if (KIND(INT(B'1011110001011001101010011111011')) /= 4) &
      error stop 13_4

      if (KIND(INT(O'13613152373')) /= 4) error stop 14_4
      if (KIND(INT(Z'5E2CD4FB')) /= 4) error stop 15_4

  end
