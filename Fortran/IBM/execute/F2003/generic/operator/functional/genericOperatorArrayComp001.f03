!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with array components
!*                                         using intrinsic operator inside defined operator function
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
         generic :: operator ( + ) => add
   end type

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a, b

      add%i = a%i + b%i

   end function

end module

program genericOperatorArrayComp001
   use m

   type (base) :: b1
   type (base), allocatable :: b2

   b1 = base ( (/ 1,2,3,4/) )

   allocate ( b2 )
   b2 = base ( (/ 5,6,7,8 /) )

   print *, b1 + b2
   print *, b1 + b1 + b2 + b2

end program