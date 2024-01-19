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
!*   test the form of ERROR STOP a_number
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_13

  SYNC ALL

  IF (THIS_IMAGE() .EQ. 1 ) THEN
    ERROR STOP 127
    ! The message on stderr will be
    ! "ERROR STOP 127"
    ! "The ERROR STOP statement with stop code 127 will terminate the program with return code 1"
    ! and the exit status will be 1.
  END IF

  SYNC ALL
  ERROR STOP "Can not reach here!"

  END

