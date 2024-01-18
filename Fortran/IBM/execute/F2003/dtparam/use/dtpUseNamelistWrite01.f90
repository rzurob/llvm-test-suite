!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseNamelistWrite01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-07
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : simple input and output
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
!*  Verify that objects are written correctly via namelist I/O.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseNamelistWrite01Basemod

  implicit none
  
  type :: Basic
     integer :: iComp = -1
  end type Basic

end module dtpUseNamelistWrite01Basemod

module dtpUseNamelistWrite01mod

  use :: dtpUseNamelistWrite01Basemod, only: Base => Basic
  implicit none

  type, extends(Base) :: Der(k)
     integer, kind :: k
     real(k) :: iCompD = -2.2
  end type

  type, extends(Der) :: Der2
     integer(k) :: iCompD2 = -3
  end type

  type, extends(Der2) :: Der3(l)
     integer, len :: l
     character(l) :: cCompD3 = 'abcdefghijklmnopqrstuvwxyz'
  end type

  type(Der3(8,9)), save  :: dmod38
  type(Der3(4,10)), save :: dmod34

end module dtpUseNamelistWrite01mod


program dtpUseNamelistWrite01

  use dtpUseNamelistWrite01mod, only: Base, Derived => Der, Derived2 => Der2, Derived3 => Der3, dmod38, dmod34
  implicit none

  type (Base)                       :: ba

  type (Derived(4))                 :: d4a
  type (Derived2(4))                :: d24a
  type (Derived3(4,3))              :: d343a

  type (Derived(8))                 :: d8a
  type (Derived2(8))                :: d28a
  type (Derived3(8,3))              :: d383a

  namelist /wDerived/ d4a, d8a
  namelist /wDerived2/ d24a, d28a
  namelist /wDerived3/ d343a, d383a, dmod34, dmod38

  namelist /wAll/ ba, d4a, d8a, d24a, d28a, d343a, d383a, dmod34, dmod38
  namelist /wFour/ ba, d4a, d24a, d343a, dmod34
  namelist /wEight/ d8a, d28a, d383a, dmod38


  read *, ba
  read *, d4a
  read *, d24a
  read *, d343a

  read *, d8a
  read *, d28a
  read *, d383a

  read *, dmod38
  read *, dmod34

  write(*, nml=wDerived)
  write(*, nml=wDerived2)
  write(*, nml=wDerived3)
  write(*, nml=wAll)
  write(*, nml=wFour)
  write(*, nml=wEight)

end program dtpUseNamelistWrite01
