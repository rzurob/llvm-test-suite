!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : error_stop_22.f
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
!*   test the form of ERROR STOP with nocaf mode
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_22

    ERROR STOP 
    ! the exit status will be 1.

    ERROR STOP "Can not reach here!"

  END 
 
