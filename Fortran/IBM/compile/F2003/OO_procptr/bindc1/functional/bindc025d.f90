!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Diagnostic: define BIND(C) procedure pointer in BIND(C)
!*                                        and non-BIND(C) common blocks
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

   use ISO_C_BINDING

   interface
      subroutine foo() bind(C)
      end subroutine
   end interface

   procedure(foo), pointer :: pp1, pp2
   integer(C_INT) :: i, j

   common /mycommon/ i, pp1

   common /bindCcommon/ j,pp2

   BIND(C) :: /bindCcommon/

end module

end
