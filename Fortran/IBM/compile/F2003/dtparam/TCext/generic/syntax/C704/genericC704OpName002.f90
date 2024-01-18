! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C704/genericC704OpName002.f
! opt variations: -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C703: unary generic operator name has to be less than 63 characters long
!*                                     contains non-letters
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      x
      contains
         generic :: operator(.abcdefghi1abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc.) => mybinaryop
         procedure :: mybinaryop
         procedure :: mybinaryop2
         generic :: operator(.abcdefghiJabcdefghiJabcdefghi#abcdefghiJabcdefghiJabcdefghiJabc.) => mybinaryop2
   end type

   contains

      type(base(4)) function mybinaryop (a, b )
         class(base(4)), intent(in) :: a, b

         mybinaryop%x = a%x + b%x
      end function

      type(base(4)) function mybinaryop2 ( b )
         class(base(4)), intent(in) :: a, b

         mybinaryop2%x = a%x + b%x
      end function

end module

program genericC704OpName002
end
