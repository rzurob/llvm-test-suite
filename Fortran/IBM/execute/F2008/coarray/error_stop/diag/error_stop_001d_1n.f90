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
!*  TEST CASE TITLE            : F2008/coarray/error_stop/diag/error_stop_001d_1.f
!*  TYPE                       : Duagnostic test
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
!*  DESCRIPTION                : Test that the error message from the 
!*                               ERROR STOP statement will be printed out 
!*                               on stderr and prefixed with "ERROR STOP".
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Before error_stop statement" 
  error stop 

  print *, " After error_stop statement" 
end program error_stop_values
