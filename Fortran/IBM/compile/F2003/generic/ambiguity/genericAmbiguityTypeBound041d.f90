!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous when optional arg is added, without optional no ambiguity
!*                               with pass dummy args
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module genericName

   type b1
      integer :: i
      contains
         procedure, pass(d) :: threeargs1
         generic :: threeargs => threeargs1
   end type

   type, extends(b1) :: c1
      contains
         procedure, pass(e) :: threeargs2
         generic :: threeargs => threeargs2
   end type

   type, extends(b1) :: c2
   end type

   contains

      subroutine threeargs1(a,b,c,d)
         class(b1), intent(in), optional :: a

         class(c1), intent(in) :: b
         class(c2), intent(in) :: c

         class(b1), intent(in) :: d

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(b,e,c)
         class(b1), intent(in) :: b
         class(b1), intent(in) :: c

         class(c1), intent(in) :: e

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound041d

! the following call is ambiguous
! call c1_1%threeargs( b=c1_1, c=c2(10) )

end program
