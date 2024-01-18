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
!*  TEST CASE TITLE            : F2008/coarray/error_stop/diag/error_stop_do_label.f
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
!*  DESCRIPTION                : Test that if a statement label is specified 
!*                               in a DO statement, ERROR STOP must not be 
!*                               used to terminate that DO construct.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program error_stop_do_label
      volatile res

      do 6789 i=1,1000
         res=1.0 + i*2
         print *, "i=", i
6789  error stop
      print *, res 
      end

