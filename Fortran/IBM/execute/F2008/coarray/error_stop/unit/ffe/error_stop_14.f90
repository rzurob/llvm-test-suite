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
!*   test the form of ERROR STOP 0
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_14

  SYNC ALL

  IF (THIS_IMAGE() .EQ. 1 ) THEN
    ERROR STOP 0
    ! The message on stderr will be
    ! "ERROR STOP 0"
    ! "The ERROR STOP statement with stop code 0 will terminate the program with return code 1"
    ! and the exit status will be 1.
  END IF

  SYNC ALL
  ERROR STOP "Can not reach here!"

  END

