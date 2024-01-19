!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         defined unary should be higher precedence than defined ** or *
!*                                         defined binary should be lower precedence than defined ** or *
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
         procedure :: myu
         generic :: operator(.u.) => myu
         procedure :: myb
         generic :: operator(.b.) => myb
         procedure :: pow
         generic :: operator(**) => pow
         procedure :: mul
         generic :: operator(*) => mul
   end type

   contains

   type(base) function myu ( a )
      class(base), intent(in) :: a

      myu = a
      myu%i = myu%i * (-1)

      print *, 'myu'

   end function

   type(base) function myb ( a, b )
      class(base), intent(in) :: a, b

      myb%i = a%i + b%i

      print *, 'myb'

   end function

   type(base) function pow ( a, b )
      class(base), intent(in) :: a, b

      pow%i = a%i ** b%i

      print *, 'pow'

   end function

   type(base) function mul ( a, b )
      class(base), intent(in) :: a, b

      mul%i = a%i * b%i

      print *, 'mul'

   end function

end module

program genericOperatorPrecedence010
   use m

   type(base) :: b1, b2, b3, b4

   b1 = .u. base(-1)
   b2 = b1 .b. base(1)
   b3 = b1 * b2
   b4 = b2 ** b3

   print *, b1, b2, b3, b4

   b1 = .u. b1 .b. b2 * b3 ** b4
   print *, b1

   b2 = b1 * b2 ** b2 .b. b3 .b. .u. b4
   print *, b2

   b3 = b4 ** b3 .b. b2 .b. .u. b1
   print *, b3

   b4 = .u. b1 .b. b2 * b3 * b4
   print *, b4

end program
