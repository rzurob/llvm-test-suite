!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSimple## (common source for dtpUseSimple01-03)
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-09-19
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : reference any module types in declarations
!*
!*  REFERENCE                  : Feature Number 355310
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
!*  One of three tests which organise the module and the program in one of three ways:
!*  1) module and program defined in the same file,
!*  2) module defined in a different file, but INCLUDED in the program file, and
!*  3) module and program defined in different files.
!*  In all cases, the program will USE the module and employ the types in declarations.
!*
!*  This is the module shared in tests 2 and 3 above.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseSimpleMod

  implicit none

  type base
     integer :: icomp
  end type base

  type, extends(base) :: kDerived(k)
     integer, kind :: k
     logical(k) :: lcomp2
  end type kDerived

  type, extends(base) :: lDerived(l)
     integer, len :: l
     character(l) :: chComp
  end type lDerived


  type dtpKBase(k)
     integer, kind :: k
     integer(k) :: icomp
  end type dtpKBase

  type, extends(dtpKBase) :: dtpKDerived
     integer(k) :: icomp2
  end type dtpKDerived

  type, extends(dtpKDerived) :: dtpK2Derived(k2)
     integer, kind :: k2
     integer(k2-k) :: icomp3
  end type dtpK2Derived


  type dtpLBase(l)
     integer, len :: l
     integer :: icomp(l)
  end type dtpLBase

  type, extends(dtpLBase) :: dtpLDerived
     character(l) :: chComp
  end type dtpLDerived

  type, extends(dtpLDerived) :: dtpL2Derived(l2)
     integer, len :: l2
     character(l2+l) :: chComp2
  end type dtpL2Derived

end module dtpUseSimpleMod
