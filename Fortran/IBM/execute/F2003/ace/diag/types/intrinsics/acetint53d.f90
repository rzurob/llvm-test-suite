!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint53d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ac-do-variable name choice
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : ac-do-variable, names
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Create AC-implied do's with inappropriate use of the loop variable: naming
!*  a variable after an intrinsic which is also used.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint53d

  implicit integer (a-z)! Normally, we want "implicit none", but this test requires implicit types
  integer :: array(8)

  ! These are all okay:
  print *, [character::  (char(int), int=iachar('a'),iachar('z'))]
  print *, [integer::    (sin, sin=1,3)]
  print *, [complex(4):: (integer*2,2**integer,integer=0,3)]
  array = [integer:: (integer, integer=1,8)]
  array = [integer:: (((i, i=1,2), j=2,3), k=4,5)]
  print *, minval([integer:: (integer, integer=1,2)])

  ! This would be okay, if "cmplx" were not used as an ac-do-variable below
  print *, [complex(4):: (cmplx(integer*2,2**integer),integer=0,3)]

  ! This would be okay, if "log" were not used as an intrinsic below
  print *, [integer(4):: (log, log=1,2)]

  ! These are bad:

  ! ac-do-variable can use any name, including that of an intrinsic or a type,
  ! with one caveat: if we want to use the name of an intrinsic, we can't use
  ! the intrinsic itself anywhere in the program, before or after
  print *, [character::  (char(cmplx), cmplx=65,70)]
  print *, [logical(4):: (logical(logical==2,2**logical),logical=0,3)]
  print *, [real(4):: (log(i/10.0), i=1,4)]

  c = count([logical:: (count==2,count=1,3)])

  array = [integer:: (cmplx, cmplx=1,8)]

end program acetint53d
