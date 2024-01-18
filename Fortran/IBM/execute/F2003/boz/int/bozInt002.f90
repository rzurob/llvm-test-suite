!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozInt002.f
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


program bozInt002

      integer(2) b2, o2, z2

      ! Two Bytes
      if (INT(B'1010110011011100') /= B'1010110011011100') &
      error stop 1_4
      
      if (INT(O'66334') /= O'66334') error stop 2_4
      if (INT(Z'6CDC') /= Z'6CDC') error stop 3_4

      b2 = INT(B'0110110011011100', 2)
      o2 = INT(O'66334', 2)
      z2 = INT(Z'6CDC', 2)

      if (b2 /= B'0110110011011100') error stop 7_4
      if (o2 /= O'66334') error stop 8_4
      if (z2 /= Z'6CDC') error stop 9_4

      if (KIND(b2) /= 2) error stop 10_4
      if (KIND(o2) /= 2) error stop 11_4
      if (KIND(z2) /= 2) error stop 12_4

      if (KIND(INT(B'1010110011011100', 2)) /= 2) error stop 13_4
      if (KIND(INT(O'126334', 2)) /= 2) error stop 14_4
      if (KIND(INT(Z'ACDC', 2)) /= 2) error stop 15_4

   end
