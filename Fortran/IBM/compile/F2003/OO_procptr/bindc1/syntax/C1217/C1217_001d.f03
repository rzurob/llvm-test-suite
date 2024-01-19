!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        C1217: Define NAME= in bind(C) statement
!*                                               and define multiple proc-decl
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

   interface
      subroutine foo() BIND(C)
      end subroutine
   end interface

   procedure(foo), BIND(C, NAME='myfptr') :: p1, p2, p3

end module

end
