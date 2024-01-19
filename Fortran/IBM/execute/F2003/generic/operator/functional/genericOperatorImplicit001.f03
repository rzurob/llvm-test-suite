!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Implicit statement with non-polymorphic types
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
      integer :: x = -999
      integer :: y = -999
      contains
         procedure, pass :: mybadd
         procedure, pass :: mybsub
         generic :: operator(+) => mybadd
         generic :: operator(-) => mybsub
   end type

   contains

   function mybadd ( a, b )
      class(base), intent(in) :: a
      type(base), intent(in)  :: b

      type(base) :: mybadd

      mybadd%x = a%x + b%x
      mybadd%y = a%y + b%y

      print *, 'mybadd'

   end function

   function mybsub ( a, b )
      class(base), intent(in) :: a
      type(base), intent(in)  :: b

      type(base) :: mybsub

      mybsub%x = a%x - b%x
      mybsub%y = a%y - b%y

      print *, 'mybsub'

   end function

end module

program genericOperatorImplicit001
   use m

   implicit type(base) (a-z)

   b1 = base ( 100, 200 )

   b3 = b1 + b2
   if ( ( b3%x /= -899 ) .or. ( b3%y /= -799 ) ) error stop 1_4

   z = b3 + b3
   if ( ( z%x /= -1798 ) .or. ( z%y /= -1598 ) ) error stop 2_4

   b2 = i - j

   if ( ( b2%x /= 0 ) .or. ( b2%y /= 0 ) ) error stop 3_4

   k = i - y + k - j

   if ( ( k%x /= 0 ) .or. ( k%y /= 0 ) ) error stop 4_4

end program
