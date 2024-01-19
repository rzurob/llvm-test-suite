!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Binary Operators (numeric and logical operators)
!*                                         Try + and .not.
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
      integer(4) :: i
      contains
         procedure :: add
         procedure :: not
         generic :: operator(+) => add
         generic :: operator(.not.) => not
   end type

   contains

   function add (a, b)
      type(base) :: add
      class(base), intent(in) :: a, b

      add%i = a%i + b%i

      print *, 'add'
   end function

   function not (a)
      type(base) :: not
      class(base), intent(in) :: a

      not%i = -1*a%i
      print *, 'not'

   end function

end module

program genericOperatorPrecedence008
   use m

   type(base) :: b1, b2, b3, b4

   b1 = .not. base(-1)
   print *, b1%i

   b2 = b1 + b1
   print *, b2%i

   b3 = .not. b2 + b1
   print *, b3%i

   b4 = .not. b1 + (.not. b2) + b3
   print *, b4%i

end program
