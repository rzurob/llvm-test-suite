!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified with multiple class hierarchy levels
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
      contains
         procedure, pass(b) :: base_base
         procedure, pass(b) :: int_base
         generic :: assignment(=) => base_base
   end type

   type, extends(base) :: child
      contains
         procedure, pass(b) :: real_child
         generic :: assignment(=) => int_base
   end type

   type, extends(child) :: gen3
      contains
         generic :: assignment(=) => real_child
   end type


   contains

   subroutine base_base ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i

      print *,'base_base'

   end subroutine

   subroutine int_base ( a, b )
      integer, intent(out) :: a
      class(base), intent(in)  :: b

      a = b%i

      print *,'int_base'

   end subroutine

   subroutine real_child ( a, b )
      real, intent(out) :: a
      class(child), intent(in)  :: b

      a = real(b%i,4)

      print *,'real_child'

   end subroutine

end module

program genericAssignmentPass013
   use m

   class(base), allocatable :: b1
   class(child), allocatable :: c1
   class(gen3), allocatable :: g1

   integer i
   real r

   allocate ( b1, c1, g1 )

   g1 = gen3(1)

   c1 = g1

   b1 = base(1)

   print *, b1%i, c1%i, g1%i

   c1%i = 2

   i = c1
   print *,i

   i = g1
   print *,i

   deallocate ( b1 )
   allocate ( b1, source = child(3) )

   select type ( b1 )
      class is ( child )
         i = b1
         print *,i
   end select

   r = g1
   print *,r

   deallocate ( c1, b1 )

   allocate ( gen3 :: c1 , b1 )

   c1 = g1
   b1 = g1

   select type ( c1 )
      class is ( gen3 )
         r = c1
         print *,r
   end select

   select type ( h => b1 )
      type is ( gen3 )
         r = h
         print *,r
   end select

end program
