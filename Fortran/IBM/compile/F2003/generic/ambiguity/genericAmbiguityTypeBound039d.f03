!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous with optional args, same number of arguments but
!*                               optional argument is the one that distinguish the two
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
         procedure, nopass :: twoargs1
         generic :: twoargs => twoargs1
   end type

   type, extends(b1) :: c1
      contains
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs2
   end type

   contains

      subroutine twoargs1(x, y, z)
         class(b1) :: x
         type(c1)  :: y
         integer, optional, intent(in) :: z

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(x, y, z)
         type(c1) :: x
         type(c1) :: y
         class(b1), optional, intent(in) :: z

         print *, 'twoargs2'

      end subroutine

end module

program genericAmbiguityTypeBound039d
end program