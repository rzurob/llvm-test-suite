!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C704: binary generic operator name has to be less than 63 characters long
!*                                     more than 63 letters
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
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcD.) => mybinaryop
         procedure :: mybinaryop
   end type

   contains

   type(base) function mybinaryop ( a, b )
      class(base), intent(in) :: a, b
      mybinaryop%x = a%x + b%x
   end function

end module

program genericC703OpName003d
end