!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with array components
!*
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

   type inner
      integer :: i
   end type

   type base
      type(inner) :: in(4)
      contains
         procedure, pass :: add
         generic :: operator ( + ) => add
   end type

   interface operator ( + )
      module procedure iadd
   end interface

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a, b

      add%in = a%in + b%in

   end function

   type(inner) function iadd ( a, b )
      class(inner), intent(in), dimension(4) :: a,b
      dimension :: iadd(4)

      iadd%i = a%i + b%i

   end function

end module

program genericOperatorArrayComp002
   use m

   type(base) :: b1, b2

   b1 = base ( (/ inner(1), inner(2), inner(3), inner(4) /) )
   b2 = base ( (/ inner(11), inner(12), inner(13), inner(14) /) )

   b1 = b1 + b2

   print *, b1%in

   b2 = b1 + b2 + b1 + b2

   print *, b2%in
   print *, base ( (/ inner(1), inner(2), inner(3), inner(4) /) ) + b1 + base ( (/ inner(-1), inner(-2), inner(-3), inner(-4) /) ) + b2

end program
