!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         defined binary should be lower precedence than defined .EQV.
!*                                         where + should have higher precedence than .EQV.
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
         procedure :: myb
         procedure :: mybt
         generic :: operator(.b.) => myb, mybt
         procedure :: add
         generic :: operator(+) => add
         procedure :: eq
         generic :: operator(==) => eq
   end type

   contains

   type(base) function myb (a, b)
      class(base), intent(in) :: a,b
      myb%i = a%i - b%i

      print *, 'myb', myb, '=', a%i, '.b.', b%i

   end function

   logical function mybt (a, b)
      class(base), intent(in) :: a
      logical, intent(in) :: b
      mybt = b

      print *, 'mybt', mybt, '=', b

   end function

   type(base) function add (a, b)
      class(base), intent(in) :: a,b
      add%i = a%i + b%i

      print *, 'add', add, '=', a%i, '+', b%i

   end function

   logical function eq (a, b)
      class(base), intent(in) :: a,b
      eq = ( a%i == b%i )

      print *, 'eq', eq, ':', a%i, '==', b%i

   end function

end module

program genericOperatorPrecedence011
   use m

   type(base) :: b1, b2, b3, b4

   b1 = base(15) .b. base(5)
   b2 = base(10) + base(10)
   b3 = base(50) .b. base(10) + base(5) + base(5)
   b4 = base(20) + base(50) .b. base(10) + base(20)

   print *, b1
   print *, b2
   print *, b3
   print *, b4

   if ( .not. ( b1 + b2 .eq. b3 ) ) error stop 1_4
   if ( .not. ( b1 + b2 == b3 ) )   error stop 2_4

   if ( b1 .b. b2 == b3 + b4 )      error stop 3_4
   if ( b1 .b. b2 .eq. b3 + b4 )    error stop 4_4

end program
