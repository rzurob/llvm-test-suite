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
!*  DESCRIPTION                : C703: Binary generic operator name has to be less than 63 characters long
!*                                     exactly 63 letters long
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

   type base
      integer x
      contains
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc.) => mybinaryop
         procedure :: mybinaryop
   end type

   contains

      type(base) function mybinaryop ( a, b )
         class(base), intent(in) :: a, b

         mybinaryop%x = a%x + b%x
      end function

end module

program genericC704OpName001
   use m

   type(base) :: a, b

   a = base(10)
   b = base(20)

   a = a .abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc. b

end
