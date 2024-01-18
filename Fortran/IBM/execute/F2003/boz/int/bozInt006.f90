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


program bozInt006

      integer(8) b8, o8, z8

      ! Six Bytes
      if (INT(B'101110000101110100111100111110010001010000001010', &
      8) /= 202710594425866_8) error stop 1_4

      if (INT(O'5605647476212012', 8) /= 202710594425866_8) &
      error stop 2_4

      if (INT(Z'B85D3CF9140A', 8) /= 202710594425866_8) error stop 3_4

      if (INT(B'101110000101110100111100111110010001010000001010') &
      /= B'101110000101110100111100111110010001010000001010') &
      error stop 4_4

      if (INT(O'5605647476212012') /= O'5605647476212012') error stop 5_4
      if (INT(Z'B85D3CF9140A') /= Z'B85D3CF9140A') error stop 6_4

      b8 = INT(B'101110000101110100111100111110010001010000001010', 8)
      o8 = INT(O'5605647476212012', 8)
      z8 = INT(Z'B85D3CF9140A', 8)

      if (b8 /= 202710594425866_8) error stop 7_4
      if (o8 /= 202710594425866_8) error stop 8_4
      if (z8 /= 202710594425866_8) error stop 9_4
      if (b8 /= B'101110000101110100111100111110010001010000001010') &
      error stop 10_4

      if (o8 /= O'5605647476212012') error stop 11_4
      if (z8 /= Z'B85D3CF9140A') error stop 12_4

      if (KIND(b8) /= 8) error stop 13_4
      if (KIND(o8) /= 8) error stop 14_4
      if (KIND(z8) /= 8) error stop 15_4

      if (KIND(INT(B'101110000101110100111100111110010001010000001010', &
      8)) /= 8) error stop 16_4

      if (KIND(INT(O'5605647476212012', 8)) /= 8) error stop 17_4
      if (KIND(INT(Z'B85D3CF9140A', 8)) /= 8) error stop 18_4

   end
