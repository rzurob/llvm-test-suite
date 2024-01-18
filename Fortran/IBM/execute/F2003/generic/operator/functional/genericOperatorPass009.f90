!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with two different types pointing to same functions (ambiguous in the child type)
!+  KEYWORD(S)                 :
!+  TARGET(S)                  :
!+ ===================================================================
!+
!+  REVISION HISTORY
!+
!+  MM/DD/YY:  Init:  Comments:
!+ ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer :: i = -999
      contains
         procedure, pass(b) :: g => int_base1_base
         generic :: operator(+) => g
   end type

   type, extends(base) :: child
      contains
         procedure, pass(b) :: g => int_base1_child
   end type

   type base1
      integer :: j  = -999
      contains
         procedure, pass :: f => int_base1_base
         generic :: operator(+) => f
   end type

   type, extends(base1) :: child1
      contains
         procedure, pass :: f => int_child1_base
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1), intent(in) :: a
      class(base), intent(in) :: b

      int_base1_base = a%j + b%i

      print *, 'int_base1_base'

   end function

   integer function int_base1_child ( a, b )
      class(base1), intent(in) :: a
      class(child), intent(in) :: b

      int_base1_child = a%j + b%i
      print *, 'int_base1_child'

   end function

   integer function int_child1_base ( a, b )
      class(child1), intent(in) :: a
      class(base), intent(in) :: b

      int_child1_base = a%j + b%i
      print *, 'int_child1_base'

   end function

end module

program genericOperatorPass009d
use m
    type(base) :: b = base(10)
    type(base1) :: b1 = base1(20)
    type(child) :: c = child(30)
    type(child1) :: c1 = child1(60)
    integer :: r
    r = b1+b
    print *,r
    r = b1+c
    print *,r
    r = c1+b
    print *,r
end program
