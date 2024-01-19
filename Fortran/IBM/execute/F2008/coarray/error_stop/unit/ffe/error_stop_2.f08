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
!*   functional test on stop code  on nocaf mode
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_2

  INTEGER, SAVE :: s = -1
  INTEGER, SAVE :: arr(3) = -1

  IF ( s .NE. -1 ) ERROR STOP "Initial value wrong!"
  ! If the initialization for s is wrong, the message on stderr will be
  ! "ERROR STOP Initial value wrong!",
  ! and the exit status will be 1.

  IF ( ANY(arr .NE. -1) ) ERROR STOP
  ! If the initialization for arr is wrong, there is no message on stderr,
  ! and the exit status will be 1.

  s = 1
  arr = 1

  IF ( s .NE. 1 ) ERROR STOP 127
  ! If the value for s is not 1, the message on stderr will be
  ! "ERROR STOP 127",
  ! and the exit status will be 127.

  IF ( ANY(arr .NE. 1) ) ERROR STOP 0
  ! If the values for arr are not 1, the message on stderr will be
  ! "ERROR STOP 0",
  ! and the exit status will be 0.

  STOP "Good!"

  END

