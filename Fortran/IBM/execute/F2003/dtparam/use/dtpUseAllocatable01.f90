!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseAllocatable01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-07
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : allocatable with no type specifier
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
!*  
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseAllocatable01Basemod

  implicit none
  
  type :: Base
     integer :: iComp
  end type Base

end module dtpUseAllocatable01Basemod

module dtpUseAllocatable01mod

  use :: dtpUseAllocatable01Basemod, only: Base
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
  end type

end module dtpUseAllocatable01mod


program dtpUseAllocatable01

  use dtpUseAllocatable01mod
  implicit none

  type (Base)                       :: ba
  type (Base), allocatable          :: bb, bc, barr(:)

  type (Derived(4))                 :: d4a
  type (Derived(4)), allocatable    :: d4b, d4c, d4arr(:)
  type (Derived2(4))                :: d24a
  type (Derived2(4)), allocatable   :: d24b, d24arr(:)
  type (Derived3(4,3))              :: d343a
  type (Derived3(4,3)), allocatable :: d343b, d343c, d343arr(:)

  type (Derived(8))                 :: d8a
  type (Derived(8)), allocatable    :: d8b, d8c, d8arr(:)
  type (Derived2(8))                :: d28a
  type (Derived2(8)), allocatable   :: d28b, d28arr(:)
  type (Derived3(8,3))              :: d383a
  type (Derived3(8,3)), allocatable :: d383b, d383c, d383arr(:)
  
  allocate(bb,d4b,d24b,d343b,d8b,d28b,d383b)

  allocate(bc, source = Base(39))
  allocate(d4c, source = Derived(4)(1209, 1721))
  allocate(d343c, source = Derived3(4,3)(1123119, 119244222, 152526242, "not yet done"))

  ! assign
  ba = Base(37)

  d4a   = Derived(4)(42,45)
  d24a  = Derived2(4)(142 , 56, 1234)
  d343a = Derived3(4,3)(11142, 11242, 15242, "this is a test")
  d8a   = Derived(8)(82, 85)
  d28a  = Derived2(8)(182, 567324_8, 1238787)
  d383a = Derived3(8,3)(11182897, 112821_8, 15282234, "wowee")

  ! auto-allocate
  d8c   = Derived(8)(353689, 17452771_8)
  d383c = Derived3(8,3)(118898333, 1198228293_8, 1303132345, "done now.")

  print *, ba

  print *, d4a, d4a % k
  print *, d24a, d24a % k
  print *, d343a, d343a % k, d343a % l
  print *, d8a, d8a % k
  print *, d28a, d28a % k
  print *, d383a, d383a % k, d383a % l

  print *, d8c, d8c % k
  print *, d383c, d383c % k, d383c % l

  allocate(barr(3), d4arr(3), d24arr(2), d343arr(3), d8arr(3), d28arr(2), d383arr(3))

  print *, size(barr)
  print *, size(d4arr), d4arr % k
  print *, size(d24arr), d24arr % k
  print *, size(d343arr), d343arr % k, d343arr % l
  print *, size(d8arr), d8arr % k
  print *, size(d28arr), d28arr % k
  print *, size(d383arr), d383arr % k, d383arr % l

  bb    = Base(38)
  d4b   = Derived(4)(19, 17)
  d24b  = Derived2(4)(119, 29, 1293)
  d343b = Derived3(4,3)(11119, 11942, 15242, "another test")
  d8b   = Derived(8)(89, 174571_8)
  d28b  = Derived2(8)(189, 293487_8, 1293656)
  d383b = Derived3(8,3)(11889876, 119823_8, 15282345, "almost done.")

  barr    = [ba, bb, bc]
  d4arr   = [d4a, d4b, d4c]
  d24arr  = [d24a, d24b]
  d343arr = [d343a, d343b, d343c]
  d8arr   = [d8a, d8b, d8c]
  d28arr  = [d28a, d28b]
  d383arr = [d383a, d383b, d383c]

  print *, size(barr), barr
  print *, size(d4arr), d4arr % k, d4arr
  print *, size(d24arr), d24arr % k, d24arr
  print *, size(d343arr), d343arr % k, d343arr % l, d343arr
  print *, size(d8arr), d8arr % k, d8arr
  print *, size(d28arr), d28arr % k, d28arr
  print *, size(d383arr), d383arr % k, d383arr % l, d383arr

end program dtpUseAllocatable01
