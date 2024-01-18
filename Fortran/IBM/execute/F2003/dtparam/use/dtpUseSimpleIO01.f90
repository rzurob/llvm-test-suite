!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSimpleIO01
!*
!*  DATE                       : 2008-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : simple input and output
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that objects are read in correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseSimpleIO01Basemod

  implicit none

  type :: Basic
     integer :: iComp = -1
  end type Basic

end module dtpUseSimpleIO01Basemod

module dtpUseSimpleIO01mod

  use :: dtpUseSimpleIO01Basemod, only: Base => Basic
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

end module dtpUseSimpleIO01mod


program dtpUseSimpleIO01

  use dtpUseSimpleIO01mod, only: Base, Derived => Der, Derived2 => Der2, Derived3 => Der3, dmod38, dmod34
  implicit none

  type (Base)                       :: ba

  type (Derived(4))                 :: d4a
  type (Derived2(4))                :: d24a
  type (Derived3(4,3))              :: d343a

  type (Derived(8))                 :: d8a
  type (Derived2(8))                :: d28a
  type (Derived3(8,3))              :: d383a

  read *, ba
  read *, d4a
  read *, d24a
  read *, d343a

  read *, d8a
  read *, d28a
  read *, d383a

  read *, dmod38
  read *, dmod34

  print *, ba

  print *, d4a, d4a % k
  print *, d24a, d24a % k
  print *, d343a, d343a % k, d343a % l
  print *, d8a, d8a % k
  print *, d28a, d28a % k
  print *, d383a, d383a % k, d383a % l

  print *, dmod38, dmod38 % k, dmod38 % l
  print *, dmod34, dmod34 % k, dmod34 % l

end program dtpUseSimpleIO01
