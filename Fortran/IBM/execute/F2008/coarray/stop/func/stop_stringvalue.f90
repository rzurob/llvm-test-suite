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
!*  TEST CASE TITLE            : F2008/coarray/stop/func/stop_stringvalue.f
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 19 Oct 2010
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that user explicitly specify non integer
!*                               but string value for STOP
!*                               then the program exit status is 0 
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Stop with string value set." 
  stop "ES_String message!"

  print *, " FAILURE if this message output!"
  sync all
end program stop_values
