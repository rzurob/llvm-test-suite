!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors within structure constructor
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
      integer :: j = -999
      contains
         procedure :: iadd
         generic :: operator(+) => iadd
   end type

   type base
      integer :: x = -999
      type(inner) :: i = inner()
      contains
         procedure :: badd
         generic :: operator (+) => badd
   end type

   interface
      type(base) function badd (a, b)
         import base
         class(base), intent(in) :: a,b
      end function
   end interface

   contains

      type(inner) function iadd ( a, b ) result(abc)
         class(inner), intent(in) :: a, b

         abc%j = a%j + b%j

      end function

end module

type(base) function badd (a, b)
   use m, only: base
   class(base), intent(in) :: a,b

   badd%x = a%x + b%x
   badd%i = a%i + b%i

end function

program genericOperatorStructConstr004
   use m

   type(base), pointer :: b1
   type(base), target :: b2

   b2 = base() + base()
   b1 => b2

   if ( ( b1%x /= -1998 ) .or. ( b1%i%j /= -1998 )      .or. &
        ( b2%x /= -1998 ) .or. ( b2%i%j /= -1998 ) )    error stop 1_4

   allocate ( b1 , source = base( 100, inner( 999 ) ) + base( 200 ) )

   if ( ( b1%x /= 300 ) .or. ( b1%i%j /= 0 ) )          error stop 2_4

   b2 = base ( 10, inner(20) ) + base (i = inner(40) , x = 30 )
   if ( ( b2%x /= 40 ) .or. ( b2%i%j /= 60 ) )          error stop 3_4

end
