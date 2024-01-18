!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Array (*,/) (assumed shape array)
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
      integer :: x = -999.9999
      contains
         procedure, pass :: arraymul
         procedure, pass, private :: scalarmul
         procedure, pass :: arraydiv
         procedure, pass, private :: scalardiv
         generic :: operator(*) => arraymul, scalarmul
         generic :: operator(/) => arraydiv, scalardiv
   end type

   contains

   function arraymul ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b(:)

      type(base), allocatable :: arraymul(:)
      allocate ( arraymul(size(b) ) )

      do i=1, size(b)
         arraymul(i) = a * b(i)
      end do

   end function

   type(base) function scalarmul ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b

      scalarmul%x = a%x * b%x

   end function

   function arraydiv ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b(:)

      type(base), allocatable :: arraydiv(:)
      allocate ( arraydiv(size(b) ) )

      do i=1, size(b)
         arraydiv(i) = a / b(i)
      end do

   end function

   type(base) function scalardiv ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b

      scalardiv%x = a%x / b%x

   end function


end module

program genericOperatorArray002
   use m

   type(base) :: b1, b2(3), b3(:)
   allocatable :: b3

   b1 = base(2)

   b2 = b1 * b1
   if ( ( b2(1)%x /= 4 ) .or. ( b2(2)%x /= 4 ) .or. ( b2(3)%x /= 4 ) )  error stop 1_4

   b2 = (/ base(1), base(2), base(3) /)

   allocate ( b3(3), source = b1 * b2 )
   if ( ( b3(1)%x /= 2 ) .or. ( b3(2)%x /= 4 ) .or. ( b3(3)%x /= 6 ) )  error stop 2_4

   do i = 1, size(b2)
      b2 = b2(i) * b3

      print *, b2%x

   end do

   b2 = b1 * b3(3:1:-1)
   if ( ( b2(1)%x /= 12 ) .or. ( b2(2)%x /= 8 ) .or. ( b2(3)%x /= 4 ) )  error stop 3_4

   b3 = b1 * b2((/ 1,3,2 /))
   if ( ( b3(1)%x /= 24 ) .or. ( b3(2)%x /= 8 ) .or. ( b3(3)%x /= 16 ) ) error stop 4_4

end program
