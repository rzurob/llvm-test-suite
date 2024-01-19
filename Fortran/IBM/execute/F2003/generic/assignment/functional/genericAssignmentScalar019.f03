!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with some more class hierarchy
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
      integer(4) :: i = -999
   end type

   type, extends(base) :: c1
      integer(4) :: j = -999
      contains
         procedure, private :: c1ab
         generic :: assignment(=) => c1ab
   end type

   type, extends(base) :: c2
      integer(4) :: j = -999
      contains
         procedure, private :: c2ab
         generic :: assignment(=) => c2ab
   end type

   contains

   subroutine c1ab ( a, b )
      class(c1), intent(out) :: a
      class(base), intent(in) :: b

      select type ( b )
         type is ( base )
            a%i = b%i
         type is ( c1 )
            a%i = b%i
            a%j = b%j
         type is ( c2 )
            a%i = b%i
            a%j = b%j
      end select

      print *, 'c1ab'

   end subroutine

   subroutine c2ab ( a, b )
      class(c2), intent(out) :: a
      class(base), intent(in) :: b

      select type ( b )
         type is ( base )
            a%i = b%i
         type is ( c1 )
            a%i = b%i
            a%j = b%j
         type is ( c2 )
            a%i = b%i
            a%j = b%j
      end select

      print *, 'c2ab'

   end subroutine

end module

program genericAssignmentScalar019
   use m

   class(base), allocatable :: b1

   type (c1) :: c11
   class(c1), allocatable :: c12

   type (c2) :: c21
   class(c2), pointer :: c22

   allocate ( c12, c22 )
   allocate ( b1, source = base(100) )

   c11 = b1
   c22 = b1

   if ( ( c11%i /= 100 ) .or. ( c11%j /= -999 ) .or. &
        ( c22%i /= 100 ) .or. ( c22%j /= -999 ) ) error stop 1_4

   c12 = c2(300, 400 )
   c21 = c1(500, 600 )

   if ( ( c12%i /= 300 ) .or. ( c12%j /= 400 ) .or. &
        ( c21%i /= 500 ) .or. ( c21%j /= 600 ) ) error stop 2_4

   c22 = c12
   c12 = c21

   if ( ( c12%i /= 500 ) .or. ( c12%j /= 600 ) .or. &
        ( c22%i /= 300 ) .or. ( c22%j /= 400 ) ) error stop 3_4

end program
