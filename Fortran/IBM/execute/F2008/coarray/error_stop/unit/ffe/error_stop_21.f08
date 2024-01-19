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
!*   test the form of ERROR STOP string with nocaf mode
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_21

    ERROR STOP "The form of ERROR STOP string in nocaf mode!"
    ! The message on stderr will be
    ! "The form of ERROR STOP string in nocaf mode!",
    ! and the exit status will be 1.

    ERROR STOP "Can not reach here!"

  END

