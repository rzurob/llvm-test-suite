!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseAllocatable02
!*
!*  DATE                       : 2008-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : allocatable with no type specifier
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
!*  Allocate derived types with a deferred parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseAllocatable02Basemod

  implicit none

  type :: Base
     integer :: iComp
  end type Base

end module dtpUseAllocatable02Basemod

module dtpUseAllocatable02mod

  use :: dtpUseAllocatable02Basemod, only: Base
  implicit none

  type, extends(Base) :: Derived(k)
     integer, kind :: k
     integer(k) :: iCompD
  end type

  type, extends(Derived) :: Derived2
     integer(k) :: iCompD2
  end type

  type, extends(Derived2) :: Derived3(l)
     integer, len :: l
     character(l) :: cCompD3
     integer(k) :: iCompD3(3) = -1
  end type

end module dtpUseAllocatable02mod


program dtpUseAllocatable02

  use dtpUseAllocatable02mod
  implicit none

  type (Derived3(4,3))              :: d343a
  type (Derived3(8,3))              :: d383a

  type (Derived3(4,:)), allocatable :: d34b, d34c, d34arr(:)
  type (Derived3(8,:)), allocatable :: d38b, d38c, d38arr(:)

  d343a = Derived3(4,3)(11142, 11242, 15242, "this is a test")
  d383a = Derived3(8,3)(11182897, 112821_8, 15282234, "wowee")

  allocate(d34b, source = d343a)
  print *, d34b % k, d34b % l, d34b
  deallocate(d34b)
  allocate(d34b, source = Derived3(4,3)(1123119, 119244222, 152526242, "not yet done"))
  print *, d34b % k, d34b % l, d34b

  allocate(d38b, source = d383a)
  print *, d38b % k, d38b % l, d38b
  deallocate(d38b)
  allocate(d38b, source = Derived3(8,3)(118898333, 1198228293_8, 1303132345, "done now."))
  print *, d38b % k, d38b % l, d38b

  ! auto-allocate

  d34c = Derived3(4,3)(11142, 11242, 15242, "this is a test")
  print *, d34c % k, d34c % l, d34c
  d38c = Derived3(8,3)(11182897, 112821_8, 15282234, "wowee")
  print *, d38c % k, d38c % l, d38c
  deallocate(d34c,d38c)

  d34c = d34b
  print *, d34c % k, d34c % l, d34c
  d38c = d38b
  print *, d38c % k, d38c % l, d38c
  deallocate(d34c,d38c)
  d34c = d343a
  print *, d34c % k, d34c % l, d34c
  d38c = d383a
  print *, d38c % k, d38c % l, d38c

  allocate(d34arr(2), source=Derived3(4,9)(11142, 11242, 15242, "brobdignag"))
  print *, d34arr % k, d34arr % l, d34arr, size(d34arr)

  d38arr = [d383a, d38b, d38c, d383a]
  print *, d38arr % k, d38arr % l, d38arr, size(d38arr)

  print *, "end"

end program dtpUseAllocatable02
