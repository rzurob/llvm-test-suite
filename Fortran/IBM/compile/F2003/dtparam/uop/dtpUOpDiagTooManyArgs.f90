!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpDiagTooManyArgs
!*
!*  DATE                       : 2009-02-09
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for > 2 args
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create a type with a user-defined operator that takes more than two
!*  arguments, and expect diagnostic messages
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagTooManyArgsmod

  implicit none
  type dk (k)
     integer, kind :: k
   contains
     procedure, pass :: manyArgProc1
     procedure, pass :: manyArgProc2
     generic :: operator(.manyarg.) => manyArgProc1, manyArgProc2
  end type dk

  type dl (l)
     integer, len :: l
   contains
     procedure, pass :: manyArgProc1L
     procedure, pass :: manyArgProc2L
     generic :: operator(.manyarg.) => manyArgProc1L, manyArgProc2L
  end type dl

contains

  integer function manyArgProc1(a1,a2,a3)
    class(dk(4)), intent(in) :: a1, a2, a3
    manyArgProc1 = 0
  end function manyArgProc1

  integer function manyArgProc2(a1,a2,a3)
    class(dk(8)), intent(in) :: a1
    integer, intent(in) :: a2, a3
    manyArgProc2 = 0
  end function manyArgProc2

  integer function manyArgProc1L(a1,a2,a3)
    class(dl(*)), intent(in) :: a1, a2, a3
    manyArgProc1L = 0
  end function manyArgProc1L

  integer function manyArgProc2L(a1,a2,a3)
    class(dl(*)), intent(in) :: a1
    integer, intent(in) :: a2, a3
    manyArgProc2L = 0
  end function manyArgProc2L

end module dtpUOpDiagTooManyArgsmod
