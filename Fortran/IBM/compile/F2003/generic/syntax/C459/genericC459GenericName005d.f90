!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within the same derived type (with deferred binding)
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

   type, abstract :: base
      integer :: id
      contains
         procedure(interface1), pass, deferred   :: printid
         procedure(interface1), nopass, deferred :: printbase
         generic :: set => printid
         generic, private :: set => printbase
   end type

   abstract interface
      subroutine interface1 (a)
         import base
         class(base), intent(in) :: a
      end subroutine

   end interface


end module

program genericC459Assignment005d
end program
