!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined unary+ should be lower precedence than defined *
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
         procedure :: unaryplus
         generic :: operator(+) => unaryplus
         procedure :: mul
         generic :: operator(*) => mul
   end type

   contains

   type(base) function unaryplus(a)
      class(base), intent(in) :: a

      if ( a%i < 0 ) then
         unaryplus%i = -1* a%i
      else
      	 unaryplus%i = a%i
      end if

      print *, 'unary+'

   end function

   type(base) function mul(a, b)
      class(base), intent(in) :: a, b
      mul%i = a%i * b%i

      print *, 'binary*'
   end function


end module

program genericOperatorPrecedence001
   use m

   type(base) :: b1, b2, b3, b4

   b1 = +base(1)
   b2 = +base(-2)

   print *, b1%i, b2%i

   b3 = b1 * b2
   b4 = b2 * b3

   print *, b3%i, b4%i

   b1 = base(-1)
   b2 = base(-2)
   b3 = base(-3)
   b4 = base(-4)

   b1 = + b1 * b2
   print *, b1%i

   b3 = + b2 * b3 * b4
   print *, b3%i

end program
