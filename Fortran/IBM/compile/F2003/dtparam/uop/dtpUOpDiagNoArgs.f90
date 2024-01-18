!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpDiagNoArgs
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-09
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for no args
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Create a type with a user-defined operator that takes no arguments, and
!*  expect diagnostic messages
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagNoArgsmod

  implicit none
  type dk (k)
     integer, kind :: k
   contains
     procedure, nopass :: noArgProc => noArgProc1
     generic :: operator(.noarg.) => noArgProc
  end type dk

  type dl (l)
     integer, len :: l
   contains
     procedure, nopass :: noArgProc => noArgProc1L
     generic :: operator(.noarg.) => noArgProc
  end type dl

contains

  integer function noArgProc1()
    noArgProc1 = 0
  end function noArgProc1

  integer function noArgProc1L()
    noArgProc1L = 0
  end function noArgProc1L

end module dtpUOpDiagNoArgsmod
