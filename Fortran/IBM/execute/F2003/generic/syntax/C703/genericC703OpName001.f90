!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C703: unary generic operator name has to be less than 63 characters long
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
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc.) => myunaryop
         procedure :: myunaryop
   end type

   contains

      type(base) function myunaryop ( b )
         class(base), intent(in) :: b

         myunaryop%x = -1* b%x
      end function

end module

program genericC703OpName001
   use m

   type(base) :: a, b

   a = base(10)
   b = base(20)

   a = .abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc. b
   if (a%x /= -20) stop 10
end
