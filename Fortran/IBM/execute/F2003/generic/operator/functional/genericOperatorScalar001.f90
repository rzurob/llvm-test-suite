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
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (+,-)
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

program genericOperatorScalar001
   use m

   type(base), target      :: b1
   type(base), pointer     :: b2
   type(base), allocatable :: b3

   b1 = base ( 100, 200 )

   allocate ( b2, source = base ( 101, 201 ) )
   allocate ( b3, source = base () )

   b3 = b1 + b2
   if ( ( b3%x /= 201 ) .or. ( b3%y /= 401 ) ) error stop 1_4

   b2 => b1

   b2 = b3 + b3
   if ( ( b2%x /= 402 ) .or. ( b2%y /= 802 ) .or. ( b1%x /= 402 ) .or. ( b1%y /= 802 ) ) error stop 2_4

   allocate ( b2, source = base ( 0, 0 ) )

   b2 = b1 - b3

   if ( ( b2%x /= 201 ) .or. ( b2%y /= 401 ) ) error stop 3_4

   b2 = b1 - b3

   if ( ( b2%x /= 201 ) .or. ( b2%y /= 401 ) ) error stop 4_4

end program
