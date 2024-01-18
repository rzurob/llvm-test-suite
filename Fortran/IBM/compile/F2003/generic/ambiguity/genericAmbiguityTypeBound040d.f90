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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : ambiguous when optional arg is added, without optional no ambiguity
!*                               
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
         procedure, nopass :: threeargs1
         generic :: threeargs => threeargs1
   end type

   type, extends(b1) :: c1
      contains
         procedure, nopass :: threeargs2
         generic :: threeargs => threeargs2
   end type
   
   type, extends(b1) :: c2
   end type

   contains

      subroutine threeargs1(x, y, z)
         class(b1), intent(out) :: x
         class(c1), intent(out) :: y
         class(b1), optional, intent(out) :: z

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(x, y, z)
         class(c2), intent(out) :: x
         class(c1), intent(out) :: y
         class(c2), intent(out) :: z

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound040d
end program
