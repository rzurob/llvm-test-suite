!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified
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
      integer :: i = -999
      contains
         procedure, pass(a) :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child
   end type

   contains

   subroutine ab ( a, b )
      class(base), intent(out)   :: a
      class(base), intent(in)   :: b

      integer, save :: i = 0

      a%i = b%i + i

      select type  ( a )
         type is ( child )
            call inner()
      end select

      i = i + 1

      contains

      subroutine inner ()
         i = i + 1
      end subroutine

   end subroutine

end module

program genericAssignmentSave002
   use m

   type(base) :: b1
   class(base), allocatable :: b2
   class(base), allocatable :: c1

   allocate ( b2, source = base(100) )

   allocate ( c1, source = child(200) )

   b1 = b2
   print *, b1

   b1 = b2
   print *, b1

   b1 = b2
   print *, b1

   b2 = b1
   print *, b2%i

   b2 = b1
   print *, b2%i

   b2 = b1
   print *, b2%i

   c1 = b1
   print *, c1%i

   b2 = b1
   print *, b2%i

end program