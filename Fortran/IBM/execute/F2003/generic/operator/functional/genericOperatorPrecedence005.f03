!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined unary + should be higher precedence than defined binary +
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
         procedure :: positive
         generic :: operator(+) => positive
         generic :: operator(+) => add
   end type

   contains


   type(base) function positive(a)
      class(base), intent(in) :: a

      if ( a%i < 0 ) then
         positive%i = -1*a%i
      else
         positive%i = a%i
      end if

      print *, positive%i, ' is positive of ', a%i

   end function

   type(base) function add(a,b)
      class(base), intent(in) :: a,b

      add%i = a%i + b%i

      print *, add%i, ' = ', a%i,' add ', b%i

   end function


end module

program genericOperatorPrecedence005
   use m

   type(base) :: b1, b2, b3, b4

   b1 = base(2) + base(2)
   b2 = +base(-3)
   print *, b1%i, b2%i

   b3 = +b1 + b2 + b2
   print *, b3%i

   b4 = b3 + b1 + (+b3 + (+b2) )
   print *, b4%i

   b3 = base(2) + (+base(-4)) + base(-6) + (+base(-8)) + (+base(-10))
   print *, b3%i

   b4 = b1 + (+base(-6)) + (+base(8)) + base(10) + (+base(-12)) + base(14)
   print *,  b4%i

end program