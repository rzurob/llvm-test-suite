!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Scalar with polymorphic inner type within types
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

   type inner1
      integer :: i
      contains
         generic, private :: operator (+) => sub1
         procedure :: sub1
   end type

   type inner2
      integer :: i
      contains
         private
         generic :: operator (+) => sub2
         procedure :: sub2
   end type

   type container
      type(inner1) :: i1
      type(inner2) :: i2
      contains
         generic :: operator (-) => xsub
         procedure :: xsub
   end type

   contains

   type(container) function xsub (a, b)
      class(container), intent(in) :: a, b
      xsub%i1 = a%i1 + b%i2
      xsub%i2 = a%i2 + b%i1
   end function

   type(inner1) function sub1 (a, b)
      class(inner1), intent(in) :: a
      type(inner2), intent(in) :: b

      sub1%i = a%i - b%i

   end function

   type(inner2) function sub2 (a, b)
      class(inner2), intent(in) :: a
      type(inner1), intent(in) :: b

      sub2%i = a%i - b%i

   end function

end module

program genericOperatorScalar010
   use m

   type(container) :: b1 = container ( inner1(10), inner2(20) )
   class(container), pointer :: b2

   if ( ( b1%i1%i /= 10 ) .or. ( b1%i2%i /= 20 ) )   error stop 1_4
   b1 = ( container ( inner1(10), inner2(20) ) - container ( inner1(10), inner2(5) ) )

   if ( ( b1%i1%i /= 5 ) .or. ( b1%i2%i /= 10 ) )    error stop 2_4

   allocate ( b2, source = b1 - b1 )
   if ( ( b2%i1%i /= -5 ) .or. ( b2%i2%i /= 5 ) )    error stop 3_4

   b1 = b2 - b2
   if ( ( b1%i1%i /= -10 ) .or. ( b1%i2%i /= 10 ) )  error stop 4_4

   b1 = b2 - b1
   if ( ( b1%i1%i /= -15 ) .or. ( b1%i2%i /= 15 ) )  error stop 5_4

end program
