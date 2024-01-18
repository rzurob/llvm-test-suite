!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with array components
!*                                         inside defined operator function, call another defined operator function
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
      integer :: i(4)
      contains
         procedure, pass :: add
         procedure, pass :: add1
         generic :: operator ( + ) => add, add1
   end type

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a, b

      add = a + b%i

   end function

   type(base) function add1 ( a, b )
      class(base), intent(in) :: a
      integer, intent(in) :: b(4)

      add1%i = a%i + b

   end function

end module

program genericOperatorArrayComp001a1
   use m

   type (base) :: b1
   type (base), allocatable :: b2

   b1 = base ( (/ 1,2,3,4/) )

   allocate ( b2 )
   b2 = base ( (/ 5,6,7,8 /) )

   print *, b1 + b2
   print *, b1 + b1 + b2 + base ( (/5,6,7,8/) )

end program
