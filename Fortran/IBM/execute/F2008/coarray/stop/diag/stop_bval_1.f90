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
!*  TEST CASE TITLE            : F2008/coarray/stop/diag/stop_bval_1.f
!*  TYPE                       : Diagnostic test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 19 Oct 2010
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that the error message is issued when 
!*                               the STOP stop code is not scalar constant 
!*                               expression of character or integer type
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_bval

  stop i
  stop k8
  stop 1 2
  stop 3k
  stop .
  stop *
  stop [i]
  stop 0xZ
  stop \t
  stop j=2
  stop -
  stop stop
  stop error stop
end program stop_bval
