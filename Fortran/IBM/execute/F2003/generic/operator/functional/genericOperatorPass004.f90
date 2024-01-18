!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: with pass attribute with another derived type
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
      integer :: i = -999
      contains
         procedure, pass(b) :: int_base1_base
         generic :: operator(*) => int_base1_base
   end type

   type base1
      integer :: i = -999
      contains
         procedure, pass(b) :: int_base_base1
         generic :: operator(*) => int_base_base1
   end type

   contains

   integer function int_base1_base ( a, b )
      type(base1), intent(in) :: a
      class(base), intent(in) :: b

      int_base1_base = a%i * b%i
      print *, 'int_base1_base'

   end function

   integer function int_base_base1 ( a, b )
      type(base), intent(in) :: a
      class(base1), intent(in) :: b

      int_base_base1 = a%i * b%i
      print *, 'int_base_base1'

   end function

end module

program genericOperatorPass004
   use m

   type(base) :: b1
   class(base1), pointer :: b11
   integer :: i

   allocate ( b11, source = base1( base(10) * base1(20) ) )
   b1 = base( ( base(2) * base1(3) ) + ( base1(4) * base(5) ) )

   print *, b11%i, b1%i

   i =( base(3) * base1(4) ) * 5 * ( base1(6) * base(7) )
   print *,i

   i = base( base( base(2) * base1(3) * 2 ) * base1 ( base1(4) * base(5) * 2 ) ) * base1( base1( base1(2) * base(3) * 2 ) * base ( base(4) * base1(5) * 2 ) )
   print *,i

end program
