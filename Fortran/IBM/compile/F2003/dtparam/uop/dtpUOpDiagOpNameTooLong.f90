!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpDiagOpNameTooLong
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-09
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic for name longer than 63 characters
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
!*  Create a type with long user-defined operator names, and expect diagnostic
!*  messages
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpDiagOpNameTooLongmod

  implicit none
  type dk (k)
     integer, kind :: k
   contains
     procedure, pass :: proc1a
     procedure, pass :: proc1b
     procedure, pass :: proc2a
     procedure, pass :: proc2b
     generic :: operator(.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.) => proc1a
     generic :: operator(.zxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.) => proc1b
     generic :: operator(.yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.) => proc2a
     generic :: operator(.zyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.) => proc2b
  end type dk

  type dl (l)
     integer, len :: l
   contains
     procedure, pass :: proc1c
     procedure, pass :: proc1d
     procedure, pass :: proc2c
     procedure, pass :: proc2d
     generic :: operator(.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.) => proc1c
     generic :: operator(.zxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.) => proc1d
     generic :: operator(.yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.) => proc2c
     generic :: operator(.zyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.) => proc2d
  end type dl

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

end module dtpUOpDiagOpNameTooLongmod
