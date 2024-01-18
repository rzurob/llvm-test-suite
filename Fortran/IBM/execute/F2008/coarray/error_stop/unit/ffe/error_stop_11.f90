!*  ===================================================================
!*
!*  DATE                       : July 30, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : error stop statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*   test the form of ERROR STOP string
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_11

  SYNC ALL

  IF (THIS_IMAGE() .EQ. 1 ) THEN
    ERROR STOP "The form of ERROR STOP string!"
    ! The message on stderr will be
    ! "The form of ERROR STOP string!",
    ! and the exit status will be 1.
  END IF

  SYNC ALL
  ERROR STOP "Can not reach here!"

  END

