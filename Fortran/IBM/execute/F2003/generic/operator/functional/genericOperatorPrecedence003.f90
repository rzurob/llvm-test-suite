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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined binary + should be lower precedence than defined *
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
         procedure :: add
         generic :: operator(+) => add
         procedure :: mul
         generic :: operator(*) => mul
   end type

   contains

   type(base) function add(a,b)
      class(base), intent(in) :: a,b

      add%i = a%i + b%i

      print *, 'add'

   end function

   type(base) function mul(a, b)
      class(base), intent(in) :: a, b

      mul%i = a%i * b%i

      print *, 'mul'

   end function


end module

program genericOperatorPrecedence003
   use m

   type(base) :: b1, b2, b3, b4

   b1 = base(2) + base(2)
   b2 = base(4) * base(2)
   print *, b1%i, b2%i

   b3 = b1 + b2 * b2
   print *, b3%i

   b4 = b3 + b1 * b3 + b2
   print *, b4%i

   b3 = base(2) + base(2) + base(2) * base(2) * base(2)
   print *, b3%i

   b4 = b1 * base(2) + base(4) * base(2) * base(1) + base(4)
   print *,  b4%i

end program
