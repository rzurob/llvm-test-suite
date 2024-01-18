!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : error_stop_12.f
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
!*   test the form of ERROR STOP 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_12

  SYNC ALL
   
  IF (THIS_IMAGE() .EQ. 1 ) THEN
    ERROR STOP 1
    ! the exit status will be 1.
  END IF
 
  SYNC ALL
  ERROR STOP "Can not reach here!"

  END 
 
