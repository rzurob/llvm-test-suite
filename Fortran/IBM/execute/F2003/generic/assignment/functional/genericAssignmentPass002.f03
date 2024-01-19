!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified, assignment to intrinsic type or to derived type
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
         procedure, pass(b) :: int_type
         procedure, pass(a) :: type_int
         generic :: assignment(=) => type_int, int_type
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
      contains
         procedure, pass(b) :: int_type => int_child
         procedure, pass(a) :: type_int => child_int
   end type

   contains

   subroutine int_type ( a, b )
      integer, intent(out)    :: a
      class(base), intent(in) :: b

      a = b%i

      print *,'int_type'

   end subroutine

   subroutine int_child ( a, b )
      integer, intent(out)    :: a
      class(child), intent(in) :: b

      a = b%i + b%j

      print *,'int_child'

   end subroutine

   subroutine type_int ( a, b )
      class(base), intent(out) :: a
      integer, intent(in)     :: b

      a%i = b

      print *,'type_int'

   end subroutine

   subroutine child_int ( a, b )
      class(child), intent(out) :: a
      integer, intent(in)     :: b

      a%i = b
      a%j = b

      print *,'child_int'

   end subroutine

end module

program genericAssignmentPass002
   use m

   class(base), allocatable :: b1
   class(child), pointer :: c1
   integer :: i

   allocate ( b1, c1 )

   b1 = 100
   print *, b1%i
   c1 = 1000
   print *, c1%i, c1%j

   i = b1
   print *,i

   i = c1
   print *,i

   deallocate ( b1 )
   allocate ( b1, source = c1 )

   b1 = 5000
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   i = b1
   print *,i

end program
