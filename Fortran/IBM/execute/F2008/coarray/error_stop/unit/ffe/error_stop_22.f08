!*  ===================================================================
!*
!*  DATE                       : July 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : error stop statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*   test the form of ERROR STOP with nocaf mode
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_22

    ERROR STOP
    ! the exit status will be 1.

    ERROR STOP "Can not reach here!"

  END

