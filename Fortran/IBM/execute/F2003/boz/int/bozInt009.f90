!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozInt009.f
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


program bozInt009

      integer b2, o2, z2

      ! Two Bytes
      if (INT(B'10110011011100') /= 11484) error stop 1_4
      if (INT(O'26334') /= 11484) error stop 2_4
      if (INT(Z'2CDC') /= 11484) error stop 3_4
      if (INT(B'10110011011100') /= B'10110011011100') error stop 1_4
      if (INT(O'26334') /= O'26334') error stop 2_4
      if (INT(Z'2CDC') /= Z'2CDC') error stop 3_4

      b2 = INT(B'0110110011011100')
      o2 = INT(O'66334')
      z2 = INT(Z'6CDC')

      if (b2 /= 27868) error stop 7_4
      if (o2 /= 27868) error stop 8_4
      if (z2 /= 27868) error stop 9_4
      if (b2 /= B'0110110011011100') error stop 7_4
      if (o2 /= O'66334') error stop 8_4
      if (z2 /= Z'6CDC') error stop 9_4

      if (KIND(b2) /= 2) error stop 10_4
      if (KIND(o2) /= 2) error stop 11_4
      if (KIND(z2) /= 2) error stop 12_4

      if (KIND(INT(B'1010110011011100')) /= 2) error stop 13_4
      if (KIND(INT(O'126334')) /= 2) error stop 14_4
      if (KIND(INT(Z'ACDC')) /= 2) error stop 15_4

   end
