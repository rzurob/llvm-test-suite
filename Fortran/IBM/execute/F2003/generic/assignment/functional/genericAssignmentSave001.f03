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

   contains

   subroutine ab ( a, b )
      class(base), intent(out)   :: a
      class(base), intent(in)   :: b

      integer, save :: i = 0

      a%i = b%i + i

      i = i + 1

      print *,'ab'

   end subroutine

end module

program genericAssignmentSave001
   use m

   type(base) :: b1
   class(base), allocatable :: b2

   allocate ( b2, source = base(100) )

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

end program