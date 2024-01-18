!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C460: specific-binding exists in parent type,
!*                                     and parent type cannot use generic operator
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
         procedure, pass :: add
   end type

   type, extends(base) :: child
      contains
         generic :: operator(+) => add
   end type

   interface
      class(base) function add(a, b)
         import base
         class(base), intent(in) :: a, b
         allocatable :: add
      end function
   end interface

end module

program genericC460Operator002d
   use m

   type(base) :: b1

   b1 = base(123) + base(246)

   b1 = b1 + base(-369)


end program

class(base) function add(a, b)
   use m, only: base
   class(base), intent(in) :: a, b
   allocatable :: add

   allocate ( add, source = a )

   add%i = add%i + b%i

   print *, 'baseadd'

end function

