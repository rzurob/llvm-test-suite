!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        C1217: Define NAME= in bind(C) statement and
!*                                               one proc-decl, and the pointer attribute
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   use ISO_C_BINDING

   interface
      subroutine foo() BIND(C)
      end subroutine
   end interface

   interface
      function boo(i) BIND(C)
         import C_INT
         integer(C_INT) :: boo
         integer(C_INT), intent(in) :: i
      end function
   end interface

   procedure(foo), BIND(C, NAME='myfptr1'), pointer :: p1
   procedure(boo), BIND(C, NAME='myfptr2'), pointer :: p2

end
