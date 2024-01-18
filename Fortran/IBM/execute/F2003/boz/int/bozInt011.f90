!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozInt011.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/23/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : INT intrinsic using KIND parameter
!*
!*  DRIVER STANZA              : xlf2003
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


program bozInt011

      integer(8) b8, o8, z8

      ! Eight Bytes
      if (INT(B'0111110011001110100101101101101101000000000101011101010101101101') &
      /= 8993291374322636141) error stop 1_4

      if (INT(O'763164555550005352555') /= 8993291374322636141) &
      error stop 2_4

      if (INT(Z'7CCE96DB4015D56D') /= 8993291374322636141) &
      error stop 3_4

      if (INT(B'0111110011001110100101101101101101000000000101011101010101101101') &
      /= B'0111110011001110100101101101101101000000000101011101010101101101') &
      error stop 4_4

      if (INT(O'763164555550005352555') /= O'763164555550005352555') &
      error stop 5_4

      if (INT(Z'7CCE96DB4015D56D') /= Z'7CCE96DB4015D56D') error stop 6_4

      b8 = INT(B'0111110011001110100101101101101101000000000101011101010101101101')
      o8 = INT(O'763164555550005352555')
      z8 = INT(Z'7CCE96DB4015D56D')

      if (b8 /= 8993291374322636141) error stop 7_4
      if (o8 /= 8993291374322636141) error stop 8_4
      if (z8 /= 8993291374322636141) error stop 9_4
      if (b8 /= &
      B'0111110011001110100101101101101101000000000101011101010101101101') &
      error stop 10_4

      if (o8 /= O'763164555550005352555') error stop 11_4
      if (z8 /= Z'7CCE96DB4015D56D') error stop 12_4

      if (KIND(b8) /= 8) error stop 13_4
      if (KIND(o8) /= 8) error stop 14_4
      if (KIND(z8) /= 8) error stop 15_4

      if (KIND(INT(B'0111110011001110100101101101101101000000000101011101010101101101')) &
      /= 8) error stop 16_4

      if (KIND(INT(O'763164555550005352555')) /= 8) error stop 17_4
      if (KIND(INT(Z'7CCE96DB4015D56D')) /= 8) error stop 18_4

   end
