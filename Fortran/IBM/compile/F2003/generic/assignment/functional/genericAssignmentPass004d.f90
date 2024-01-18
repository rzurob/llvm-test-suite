!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified assignment to different derived types
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
         procedure, pass(b) :: base1_base
         generic :: assignment(=) => base1_base
   end type

   type base1
      integer(8) :: k = -999
      contains
         procedure :: base1_base_ambig
         generic :: assignment(=) => base1_base_ambig
   end type

   contains

      subroutine base1_base ( a, b )
         class(base1), intent(out) :: a
         class(base), intent(in)   :: b

         print *, 'base1_base'

      end subroutine

      subroutine base1_base_ambig ( a, b )
         class(base1), intent(out) :: a
         class(base), intent(in)   :: b

         print *, 'base1_base_ambig'

      end subroutine

end module

program genericAssignmentPass004d
end program
