!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within same derived type ( some public and private specific tb )
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
      integer :: id
      contains
         private
         procedure, nopass, public :: set1
         procedure, nopass :: set2
         procedure, nopass, public :: set4
         procedure, nopass :: set8
         generic :: set => set1
   end type

   type, extends(base) :: child
      contains
         private
         generic :: set => set2
   end type

   type, extends(child) :: gen3
      contains
         generic, private :: set => set4, set8
   end type

   contains

      subroutine set1 ( a , i )
         class(base), intent(inout) :: a
         integer(1), intent(in) :: i

      end subroutine

      subroutine set2 ( a , i )
         class(base), intent(inout) :: a
         integer(2), intent(in) :: i

      end subroutine

      subroutine set4 ( a , i )
         class(base), intent(inout) :: a
         integer(4), intent(in) :: i

      end subroutine

      subroutine set8 ( a , i )
         class(base), intent(inout) :: a
         integer(8), intent(in) :: i

      end subroutine

end module

program genericC459Assignment004
end program