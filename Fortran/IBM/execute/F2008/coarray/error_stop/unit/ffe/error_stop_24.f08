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
!*   test the form of ERROR STOP 0 with nocaf mode
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_24

    ERROR STOP 0
    ! The message on stderr will be
    ! "ERROR STOP 0",
    ! and the exit status will be 0.

    ERROR STOP "Can not reach here!"

  END
