!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing functionality for some ISO_C_BINDING named constants
!*        C_NEW_LINE, C_BACKSPACE, C_CARRIAGE_RETURN, C_VERTICAL_TAB
!*        C_HORIZONTAL_TAB
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisonc01
   use ISO_C_BINDING

10 format("There should be one NEW LINE below",a1)
   write(*,10) C_NEW_LINE

11 format("This line is complete")
   write(*,11)

12 format("How about now?")
   write(*,12)

13 format("This line is complete",$,a1,a1,"  ",a1,a1)
   write(*,13) C_BACKSPACE, C_BACKSPACE, C_NEW_LINE, C_NEW_LINE

14 format("This line of text looks fine")
   write(*,14)
   write(*,12)

15 format("This line of text looks fine",$,a1)
   write(*,15) C_CARRIAGE_RETURN
16 format("Oops!!!.. Now it is not",a1)
   write(*,16) C_NEW_LINE

17 format("Let\'s",a1,a1,"do",a1,a1,"some",a1,a1,"tabbing",a1,a1,"around!!")
   write(*,17) "","","","","","","",""
   write(*,17) C_HORIZONTAL_TAB, C_VERTICAL_TAB, C_HORIZONTAL_TAB, &
               C_VERTICAL_TAB, C_HORIZONTAL_TAB, C_VERTICAL_TAB, &
               C_HORIZONTAL_TAB, C_VERTICAL_TAB

end program fxisonc01
