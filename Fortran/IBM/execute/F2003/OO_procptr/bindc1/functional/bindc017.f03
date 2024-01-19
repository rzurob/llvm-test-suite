!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Try procedure pointer containing C_CHAR dummy-arg
!*                                        of special semantics in C char
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

   use ISO_C_BINDING

   interface
      subroutine foo (c1, c2, c3)
         import C_CHAR
         character(kind=C_CHAR), intent(in), VALUE :: c1, c2, c3
      end subroutine
   end interface

   procedure(foo), pointer :: pp1
   pp1 => foo

   call pp1(C_NEW_LINE,C_HORIZONTAL_TAB,C_VERTICAL_TAB)

   call pp1(C_CARRIAGE_RETURN,C_BACKSPACE,C_NULL_CHAR)

   end
