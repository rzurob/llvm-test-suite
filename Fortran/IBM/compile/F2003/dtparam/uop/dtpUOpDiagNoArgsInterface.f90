!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpDiagNoArgsInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-09
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for no args (generic interface)
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
!*  Create a type with a user-defined operator (via generic interface, not type-bound)
!*  that takes no arguments, and expect diagnostic messages
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagNoArgsInterfacemod

  implicit none
  type dk (k)
     integer, kind :: k
  end type dk

  type dl (l)
     integer, len :: l
  end type dl

  interface operator(.noarg.)
     module procedure noArgProc1
     module procedure noArgProc1L
  end interface operator(.noarg.)

contains

  integer function noArgProc1()
    noArgProc1 = 0
  end function noArgProc1

  integer function noArgProc1L()
    noArgProc1L = 0
  end function noArgProc1L

end module dtpUOpDiagNoArgsInterfacemod
