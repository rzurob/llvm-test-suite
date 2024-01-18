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
!*  TEST CASE TITLE            : F2008/coarray/stop/func/stop_intvalue2.F
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 19 Oct 2010
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  DRIVER STANZA              : xlf  _r
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that on sequential (non-CAF) mode,
!*                               program exit status is the same as the
!*                               supplied stop-code value in the STOP statement
!*                               (if the stop-code is an integer)
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Error Code Value set to: ", TC_STOP_VALUE 
  stop TC_STOP_VALUE

  print *, " FAILURE if this message output!"

end program stop_values
