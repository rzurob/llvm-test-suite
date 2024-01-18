!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpDiagOpNameTooLongInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-09
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for name longer than 63 characters (generic interface)
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
!*  Create a type with long user-defined operator names (in generic interface,
!*  not type-bound), and expect diagnostic messages.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagOpNameTooLongInterfacemod

  implicit none
  type dk (k)
     integer, kind :: k
  end type dk

  type dl (l)
     integer, len :: l
  end type dl

  interface operator(.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.)
     module procedure proc1a
     module procedure proc1c
  end interface operator(.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.)

  interface operator(.yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.)
     module procedure proc2a
     module procedure proc2c
  end interface operator(.yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.)

  interface operator(.zxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.)
     module procedure proc1b
     module procedure proc1d
  end interface operator(.zxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.)

  interface operator(.zyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.)
     module procedure proc2b
     module procedure proc2d
  end interface operator(.zyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.)

contains

  integer function proc1a(a1)
    class(dk(4)), intent(in) :: a1
    proc1a = 0
  end function proc1a

  integer function proc1b(a1)
    class(dk(4)), intent(in) :: a1
    proc1b = 0
  end function proc1b

  integer function proc2a(a1,a2)
    class(dk(8)), intent(in) :: a1, a2
    proc2a = 0
  end function proc2a

  integer function proc2b(a1,a2)
    class(dk(8)), intent(in) :: a1, a2
    proc2b = 0
  end function proc2b


  integer function proc1c(a1)
    class(dl(*)), intent(in) :: a1
    proc1c = 0
  end function proc1c

  integer function proc1d(a1)
    class(dl(*)), intent(in) :: a1
    proc1d = 0
  end function proc1d

  integer function proc2c(a1,a2)
    class(dl(*)), intent(in) :: a1, a2
    proc2c = 0
  end function proc2c

  integer function proc2d(a1,a2)
    class(dl(*)), intent(in) :: a1, a2
    proc2d = 0
  end function proc2d

end module dtpUOpDiagOpNameTooLongInterfacemod
