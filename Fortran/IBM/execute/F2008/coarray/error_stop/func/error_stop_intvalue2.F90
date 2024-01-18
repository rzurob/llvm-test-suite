!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/coarray/error_stop/func/error_stop_intvalue2.F
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 28 August 2010
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  DRIVER STANZA              : xlf  _r
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that on sequential (non-CAF) mode,
!*                               program exit status is the same as the
!*                               supplied stop-code value in the ERROR STOP statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Error Code Value set to: ", TC_ES_VALUE 
  error stop TC_ES_VALUE

  print *, " FAILURE if this message output!"
end program error_stop_values
