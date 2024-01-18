!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with type within types
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

   type innerbase
      integer :: i
      contains
         generic, private :: operator(+) => inneradd
         procedure :: inneradd
   end type

   type base
      type(innerbase) :: b1
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   contains

   type(innerbase) function inneradd ( a, b )
      intent(in) :: a, b
      class(innerbase) :: a, b
      inneradd%i = a%i + b%i
   end function

   type(base) function add ( a, b )
      intent(in) :: a, b
      class(base) :: a, b
      add%b1 = a%b1 + b%b1
   end function

end module

program genericOperatorScalar008
   use m

   type(base) :: b1, b2, b3
   allocatable :: b2
   pointer :: b3

   allocate ( b2, source = base( innerbase( 100 ) ) )
   b1 = base ( innerbase(10) ) +  base ( innerbase(200) )

   if ( b1%b1%i /= 210 )  error stop 1_4

   b2 = b2 + b1
   if ( b2%b1%i /= 310 )  error stop 2_4

   allocate ( b3, source = ( b1 + b2 + b2 ) )
   if ( b3%b1%i /= 830 )  error stop 3_4

end program
