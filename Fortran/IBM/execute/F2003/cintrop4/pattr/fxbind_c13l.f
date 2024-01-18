! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: subroutine with BINd(C) attribute
!*                                with real16
!*                                Using external
!*                                subroutine,interface.Fortran calls C.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   implicit none

   interface

       subroutine extsub_real(r16) bind(c)
           real*16  r16
       end subroutine extsub_real

       subroutine extsub_comp(co32) bind(c)
           complex*32 co32
       end subroutine extsub_comp
   end interface

   logical precision_R6
   logical precision_x32



   real*16  ar16 /1600.3/, br16 /3200.6/
   complex*32  ac32 /(0.0Q0, 0.0Q0)/, bc32 /(1.0Q0, 1.0Q0)/


   call extsub_real(ar16)

      if(.not. precision_R6(ar16, br16))then
        error stop 22
      endif

   call extsub_comp(ac32)

      if(.not. precision_x32(ac32, bc32))then
        error stop 62
      endif

end
