!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: assumed-size array appearing as whole array, and
!*                                           using generic assignment tb with assumed-size array
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
      integer :: i =0
      contains
         procedure, pass :: bassgn
         procedure, pass :: bassgnwrapper
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(*)

         print *, 'bassgn'

         do j = 1, 5
            a%i = a%i + b(j)%i
         end do

      end subroutine

      subroutine bassgnwrapper ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(*)

         a = b

      end subroutine

end module

program genericAssignmentArray010d
end program
