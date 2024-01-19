!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: with pass attribute (+)
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
         procedure, pass    :: base_base_int
         procedure, pass(b) :: base_int_base
         procedure, pass(b) :: base_base_base
         generic :: operator(+) => base_base_int, base_int_base, base_base_base
   end type

   contains

   function base_base_base ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      type(base) :: base_base_base

      base_base_base%i = a%i + b%i
      print *, 'base_base_base'

   end function

   function base_int_base ( a, b )
      integer, intent(in) :: a
      class(base), intent(in)  :: b

      type(base) :: base_int_base

      base_int_base%i = a + b%i
      print *, 'base_int_base'

   end function

   type(base) function base_base_int ( a, b )
      class(base), intent(in) :: a
      integer, intent(in)  :: b

      base_base_int%i = a%i + b
      print *, 'base_base_int'

   end function

end module

program genericOperatorPass001
   use m

   type(base) :: b1
   class(base), allocatable :: b2

   allocate ( b2 , source = base(20))

   b1 = base(5) + 10
   print *,b1%i

   b1 = 50 + base(100)
   print *, b1%i

   b1 = base(100) + 200 + base(300) + 400 + 500 + base(600)
   print *, b1%i

   b1 = b2 + 40 + b1 + 60 + base(100) + 80 + base(40) + 100
   print *, b1%i

end program
