!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Try procedure pointer containing an interface that is renamed through (multiple) use-stmt
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

module m

   use ISO_C_BINDING

   interface
      subroutine printMe(i) bind(C,name="modMPrintMe")
         import C_INT
         integer(C_INT), intent(in) :: i
      end subroutine
   end interface

end module

module m1
   use m, printModM => printMe

   interface
      subroutine printMe(i) bind(C)
         import C_INT
         integer(C_INT), intent(in) :: i
      end subroutine
   end interface

end module

program bindc020
   use m1, m1printme => printMe, mprintme => printModM

   integer(C_INT) :: i1 = 1234567_C_INT

   procedure(mprintme), pointer, bind(C)  :: pp1
   procedure(m1printme), pointer, bind(C) :: pp2

   pp1 => mprintme
   pp2 => m1printme

   call pp1(i1)
   call pp2(i1)

end program
