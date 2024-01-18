!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined ** should be higher precedence than defined /
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
      integer i
      contains
         procedure :: pow
         generic :: operator(/) => div
         procedure :: div
         generic :: operator(**) => pow
   end type

   contains

   type(base) function pow(a,b)
      class(base), intent(in) :: a,b

      pow%i = a%i ** b%i

      print *, 'pow'

   end function

   type(base) function div(a, b)
      class(base), intent(in) :: a, b

      div%i = a%i / b%i

      print *, 'div'

   end function


end module

program genericOperatorPrecedence002
   use m

   type(base) :: b1, b2, b3, b4

   b1 = base(2) ** base(2)
   b2 = base(4) / base(2)
   print *, b1%i, b2%i

   b3 = b1 ** b2 / b2 ! 16 / 2
   print *, b3%i

   b4 = b3 ** b1 / b3 ** b2    ! ( 8 ** 4 ) / ( 8 ** 2 )
   print *, b4%i

   b3 = base(2) ** base(2) ** base(2) / base(2) ** base(2) ! ( 2 ** 2 ** 2 ) / ( 2 ** 2 )
   print *, b3%i

   b4 = b1 ** base(2) / base(4) ** base(2) / base(1) ** base(4)
   print *,  b4%i

end program
