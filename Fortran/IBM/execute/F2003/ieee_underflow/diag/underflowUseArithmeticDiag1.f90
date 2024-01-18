! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 15 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control()
!*                             : ieee_support_underflow_control(X)
!*  SECONDARY FUNCTIONS TESTED : ieee_set_underflow_mode(gradual)
!*                               ieee_get_underflow_mode(gradual)
!*  REFERENCE                  : Feature Number 289080
!*
!*  DESCRIPTION                :
!*  test if compiler issues a suitable error message when above intrinsics are referenced in program without using intrinsic ieee_arithmetic module, currently compiler will successfully let following program pass, defect 342560 was opened to track this, there is no verification file now
!*
!23456789012345678901234567890123456789012345678901234567890123456789012
      program underflowUseArithmeticDiag1
        implicit none
        real    :: r
        logical :: currentMode
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(.true.)
           call ieee_get_underflow_mode(currentMode)
        endif
       end program
