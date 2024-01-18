!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: Elemental and with pass attribute
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
         procedure, pass(b) ::  int_int_base
         procedure, pass ::  int_base_base
         generic :: operator(+) => int_base_base, int_int_base
   end type

   contains

   elemental integer function int_base_base ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      int_base_base = a%i + b%i + 1

   end function

   elemental integer function int_int_base ( a, b )
      integer, intent(in)     :: a
      class(base), intent(in) :: b

      int_int_base = a + b%i + 1

   end function

end module

program genericOperatorPass011
   use m

   integer :: i(5) = (/ 1,2,3,4,5 /)
   integer :: j(2,2)

   class(base), allocatable :: b1(:), b2(:,:)

   j = reshape ( source = i(1:4), shape = (/2,2/) )
   allocate ( b1(size(i)), source = (/ base(base(5)+base(5)), base(base(10)+base(10)), base(base(15)+base(15)), base(base(20)+base(20)), base(base(25)+base(25)) /) )
   allocate ( b2(size(j,1), size(j,2)), source = reshape ( source = (/ base(base(5)+base(5)), base(base(10)+base(10)), base(base(15)+base(15)), base(base(20)+base(20)) /) , shape = (/2,2/) ) )

   print *, b1%i
   print *, b2%i

   i = i + b1
   print *,i

   j = j + b2
   print *, j

end program
