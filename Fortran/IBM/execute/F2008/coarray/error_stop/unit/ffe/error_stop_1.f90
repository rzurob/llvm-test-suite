!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : error_stop_1.f
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : July 30, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : error stop statement 
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*   functional test on stop code  on caf mode
!*
!*  (380368)
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_1

  INTEGER, SAVE :: scoarr[*] = -1
  INTEGER, SAVE :: acoarr(3)[*] = -1
   
  IF ( scoarr .NE. -1 ) ERROR STOP "Initial value wrong!"
  ! If the initialization for scoarr is wrong, the message on stderr will be 
  ! "ERROR STOP Initial value wrong!",
  ! and the exit status will be 1.
   
  IF ( ANY(acoarr[THIS_IMAGE()] .NE. -1) ) ERROR STOP
  ! If the initialization for acoarr is wrong, there is no message on stderr,
  ! and the exit status will be 1.
   
  SYNC ALL

  IF (THIS_IMAGE() .EQ. NUM_IMAGES()) THEN
    scoarr[1] = 0
    acoarr[1] = 0
  ELSE
    scoarr[THIS_IMAGE()+1] = THIS_IMAGE()
    acoarr[THIS_IMAGE()+1] = 0
  END IF
   
  SYNC ALL
   
  IF ( scoarr .NE. THIS_IMAGE()-1 ) ERROR STOP 127
  ! If the value for scoarr is not equal to THIS_IMAGE()-1, the message on stderr will be
  ! "ERROR STOP 127"
  ! "The ERROR STOP statement with stop code 127 will terminate the program with return code 1"
  ! and the exit status will be 1.
  
  IF ( ANY(acoarr .NE. 0) ) ERROR STOP 0
  ! If the values for acoarr are not 0, the message on stderr will be
  ! "ERROR STOP 0"
  ! "The ERROR STOP statement with stop code 0 will terminate the program with return code 1"
  ! and the exit status will be 1.
   
  STOP "Good!"
   
  END 
 
