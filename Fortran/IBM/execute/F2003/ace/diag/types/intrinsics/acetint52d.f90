!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint52d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ac-do-variable name scope
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : ac-do-variable, scope
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Create nested AC-implied do's, defining the same loop variable twice.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint52d

  implicit none
  integer :: i, j, k, array(8), c
  real    :: rarray(8)

  ! These are all okay:
  rarray = [real:: (((sin(real(i*j*k)), i=1,2), j=2,3), k=4,5)]
  array  = [integer:: (((i*j*k, i=1,2), j=2,3), k=4,5)]
  c = count([logical:: (((i==j, i=1,2), j=2,3), k=4,5)])
  print *, minval([integer:: (((i*j*k, i=1,2), j=2,3), k=4,5)])

  ! These are bad - an implied-do defining an ac-do-variable "var" cannot contain
  ! another implied-do defining a variable of the same name:
  print *, [real(4):: (((real(i)*real(j), i=1,3),j=1,2),i=1,1)] ! "i" used twice
  print *, [real(4):: (((real(i)*real(j), i=1,3),j=1,2),j=1,1)] ! "j" used twice

  array = [integer:: (((i, i=1,2), j=2,3), i=4,5)]
  array = [integer:: (((i, i=1,2), j=2,3), j=4,5)]

  c = count([logical:: (((i==j, i=1,2), j=2,3), i=4,5)])
  c = count([logical:: (((i==j, i=1,2), j=2,3), j=4,5)])

end program acetint52d
