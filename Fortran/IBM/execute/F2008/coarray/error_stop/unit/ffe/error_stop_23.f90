!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : error_stop_23.f
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
!*   test the form of ERROR STOP a_number with nocaf mode
!*  (380387)
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_23

    ERROR STOP 127 
    ! The message on stderr will be
    ! "ERROR STOP 127",
    ! and the exit status will be 127.

    ERROR STOP "Can not reach here!"

  END 
 
