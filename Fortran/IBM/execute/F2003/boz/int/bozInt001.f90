!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozInt001.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : INT intrinsic
!*
!*  DRIVER STANZA              : xlf2003
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


program bozInt001

      integer(1) b1, o1, z1

      ! One Byte
      if (INT(B'11101', 1) /= B'11101') error stop 1_4
      if (INT(O'35', 1) /= O'35') error stop 2_4
      if (INT(Z'1D', 1) /= Z'1D') error stop 3_4

      b1 = INT(B"11101", 1)
      o1 = INT(O"35", 1)
      z1 = INT(Z"1D", 1)

      if (b1 /= B'11101') error stop 7_4
      if (o1 /= O'35') error stop 8_4
      if (z1 /= Z'1D') error stop 9_4

      if (KIND(b1) /= 1) error stop 10_4
      if (KIND(o1) /= 1) error stop 11_4
      if (KIND(z1) /= 1) error stop 12_4

      if (KIND(INT(B'11101', 1)) /= 1) error stop 13_4
      if (KIND(INT(O'35', 1)) /= 1) error stop 14_4
      if (KIND(INT(Z'1D', 1)) /= 1) error stop 15_4

   end
