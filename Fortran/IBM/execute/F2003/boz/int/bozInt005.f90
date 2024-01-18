!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozInt005.f
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


program bozInt005

      integer(8) b8, o8, z8

      ! Five Bytes
      if (INT(B'1100001011010011101100010110111011110100', 8) /= &
      B'1100001011010011101100010110111011110100') error stop 1_4

      if (INT(O'14132354267364') /= O'14132354267364') error stop 2_4
      if (INT(Z'C2D3B16EF4') /= Z'C2D3B16EF4') error stop 3_4

      b8 = INT(B'1100001011010011101100010110111011110100', 8)
      o8 = INT(O'14132354267364', 8)
      z8 = INT(Z'C2D3B16EF4', 8)

      if (b8 /= B'1100001011010011101100010110111011110100') &
      error stop 7_4

      if (o8 /= O'14132354267364') error stop 8_4
      if (z8 /= Z'C2D3B16EF4') error stop 9_4

      if (KIND(b8) /= 8) error stop 10_4
      if (KIND(o8) /= 8) error stop 11_4
      if (KIND(z8) /= 8) error stop 12_4

      if (KIND(INT(B'1100001011010011101100010110111011110100', 8)) &
      /= 8) error stop 1_4

      if (KIND(INT(O'14132354267364', 8)) /= 8) error stop 2_4
      if (KIND(INT(Z'C2D3B16EF4', 8)) /= 8) error stop 3_4

   end
