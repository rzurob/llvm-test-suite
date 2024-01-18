!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with another derived type (ambiguous)
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
         procedure, pass(b) :: int_base1_base
         generic :: operator(+) => int_base1_base
   end type

   type base1
      integer :: j = -999
      contains
         procedure, pass :: int_base1_base_amb
         generic :: operator(+) => int_base1_base_amb
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1), intent(in) :: a
      class(base), intent(in) :: b
      int_base1_base=10
   end function

   integer function int_base1_base_amb ( a, b )
      class(base1), intent(in) :: a
      class(base), intent(in) :: b
      int_base1_base_amb=20
   end function

end module

program genericOperatorPass007d
end program
