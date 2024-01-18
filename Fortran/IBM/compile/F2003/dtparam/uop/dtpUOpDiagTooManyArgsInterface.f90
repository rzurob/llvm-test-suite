!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-09
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for > 2 args (generic interface)
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
!*  Create a type with a user-defined operator (with generic interface, not
!*  type-bound) that takes more than two arguments, and expect diagnostic messages.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagTooManyArgsInterfacemod

  implicit none
  type dk (k)
     integer, kind :: k
  end type dk

  type dl (l)
     integer, len :: l
  end type dl

  interface operator(.manyarg.)
     module procedure manyArgProc1
     module procedure manyArgProc2
     module procedure manyArgProc1L
     module procedure manyArgProc2L
  end interface operator(.manyarg.)

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

end module dtpUOpDiagTooManyArgsInterfacemod
