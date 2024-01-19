!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : INT intrinsic
!*
!*  DESCRIPTION                : boz-literal-constant treated as an
!*                              int-literal-constant when passed as
!*                              parameter A in the INT intrinsic.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program bozInt003

      integer(4) b4, o4, z4

      ! Three Bytes
      if (INT(B'110000011010011110101111') /= 12691375) error stop 1_4
      if (INT(O'60323657') /= 12691375) error stop 2_4
      if (INT(Z'C1A7AF') /= 12691375) error stop 3_4
      if (INT(B'110000011010011110101111') /= &
      B'110000011010011110101111') error stop 1_4

      if (INT(O'60323657') /= O'60323657') error stop 2_4
      if (INT(Z'C1A7AF') /= Z'C1A7AF') error stop 3_4

      b4 = INT(B'110000011010011110101111', 4)
      o4 = INT(O'60323657', 4)
      z4 = INT(Z'C1A7AF', 4)

      if (b4 /= 12691375) error stop 7_4
      if (o4 /= 12691375) error stop 8_4
      if (z4 /= 12691375) error stop 9_4
      if (b4 /= B'110000011010011110101111') error stop 7_4
      if (o4 /= O'60323657') error stop 8_4
      if (z4 /= Z'C1A7AF') error stop 9_4

      if (KIND(b4) /= 4) error stop 10_4
      if (KIND(o4) /= 4) error stop 11_4
      if (KIND(z4) /= 4) error stop 12_4

      if (KIND(INT(B'110000011010011110101111')) /= 4) error stop 13_4
      if (KIND(INT(O'60323657')) /= 4) error stop 14_4
      if (KIND(INT(Z'C1A7AF')) /= 4) error stop 15_4

   end
