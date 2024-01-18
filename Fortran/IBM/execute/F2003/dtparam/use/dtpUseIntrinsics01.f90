!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseIntrinsics01
!*
!*  DATE                       : 2008-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : use intrinsic functions relevant to DTP
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
!*  Programs will specify types and define variables which are then passed to
!*  instrinsics and the results tested. Note that SAME_TYPE_AS(A,B) "is true if
!*  and only if the dynamic type of A is the same as the dynamic type of B",
!*  which implies that two parameterised DT's of the same type but with a
!*  different kind should be considered to be the same type.  Similarly,
!*  EXTENDS_TYPE_OF(A,MOLD) tests if the dynamic type of A is an extension type
!*  of the dynamic type of MOLD, but again without regard to kind (or rank).
!*  We will test a weaker condition, and consider kind in the test, i.e., we
!*  will not insist that SAME_TYPE_AS(Derived(4)(xx),Derived(8)(yy)) be true,
!*  just that SAME_TYPE_AS(Derived(4)(xx),Derived(4)(yy)) be true.
!*  We do not test the use of TRANSFER here.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseIntrinsics01BaseMod

  implicit none

  type :: Base
     integer :: iComp = -1
  end type Base

end module dtpUseIntrinsics01BaseMod

module dtpUseIntrinsics01Mod

  use :: dtpUseIntrinsics01BaseMod, only: Base
  implicit none

  type, extends(Base) :: Derived(k)
     integer, kind :: k
     integer(k) :: iCompD = -2
  end type

  type, extends(Derived) :: Derived2
     integer(k) :: iCompD2 = -3
  end type

  type, extends(Derived2) :: Derived3(l)
     integer, len :: l
     character(l) :: cCompD3 = 'zyxwvutsrqponmlkjihgfedcba'
  end type

end module dtpUseIntrinsics01Mod

program dtpUseIntrinsics01

  use dtpUseIntrinsics01mod
  implicit none

  type (Base)                       :: ba, bd
  type (Base), allocatable          :: bb, barr(:)
  class (Base), allocatable         :: bc

  type (Derived(4))                 :: d4a, d4d
  type (Derived(4)), allocatable    :: d4b, d4arr(:)
  class(Derived(4)), allocatable    :: d4c

  type (Derived2(4))                :: d24a, d24d
  type (Derived2(4)), allocatable   :: d24b, d24arr(:)
  class(Derived2(4)), allocatable   :: d24c

  type (Derived3(4,3))              :: d343a, d343d
  type (Derived3(4,3)), allocatable :: d343b, d343arr(:)
  class(Derived3(4,3)), allocatable :: d34c

  type (Derived(8))                 :: d8a, d8d
  type (Derived(8)), allocatable    :: d8b, d8arr(:)
  class(Derived(8)), allocatable    :: d8c

  type (Derived2(8))                :: d28a, d28d
  type (Derived2(8)), allocatable   :: d28b, d28arr(:)
  class(Derived2(8)), allocatable   :: d28c

  type (Derived3(8,3))              :: d383a, d383d
  type (Derived3(8,3)), allocatable :: d383b, d383arr(:)
  class(Derived3(8,3)), allocatable :: d38c

  ! static types:
  if (.not. (same_type_as(ba,bd) .and. same_type_as(bd,ba))) stop 10
  if (.not. (same_type_as(d4a,d4d) .and. same_type_as(d4d,d4a))) stop 11
  if (.not. (same_type_as(d24a,d24d) .and. same_type_as(d24d,d24a))) stop 12
  if (.not. (same_type_as(d343a,d343d) .and. same_type_as(d343d,d343a))) stop 13
  if (.not. (same_type_as(d8a,d8d) .and. same_type_as(d8d,d8a))) stop 14
  if (.not. (same_type_as(d28a,d28d) .and. same_type_as(d28d,d28a))) stop 15
  if (.not. (same_type_as(d383a,d383d) .and. same_type_as(d383d,d383a))) stop 16

  if (.not. (extends_type_of(ba,bd) .and. extends_type_of(bd,ba))) stop 17
  if (.not. (extends_type_of(d4a,d4d) .and. extends_type_of(d4d,d4a))) stop 18
  if (.not. (extends_type_of(d24a,d24d) .and. extends_type_of(d24d,d24a))) stop 19
  if (.not. (extends_type_of(d343a,d343d) .and. extends_type_of(d343d,d343a))) stop 20
  if (.not. (extends_type_of(d8a,d8d) .and. extends_type_of(d8d,d8a))) stop 21
  if (.not. (extends_type_of(d28a,d28d) .and. extends_type_of(d28d,d28a))) stop 22
  if (.not. (extends_type_of(d383a,d383d) .and. extends_type_of(d383d,d383a))) stop 23

  ! static types and unallocated allocatables:
  if (.not. (same_type_as(ba,bb) .and. same_type_as(bb,ba))) stop 24
  if (.not. (same_type_as(d4a,d4b) .and. same_type_as(d4b,d4a))) stop 25
  if (.not. (same_type_as(d24a,d24b) .and. same_type_as(d24b,d24a))) stop 26
  if (.not. (same_type_as(d343a,d343b) .and. same_type_as(d343b,d343a))) stop 27
  if (.not. (same_type_as(d8a,d8b) .and. same_type_as(d8b,d8a))) stop 28
  if (.not. (same_type_as(d28a,d28b) .and. same_type_as(d28b,d28a))) stop 29
  if (.not. (same_type_as(d383a,d383b) .and. same_type_as(d383b,d383a))) stop 30

  if (.not. (extends_type_of(ba,bb) .and. extends_type_of(bb,ba))) stop 31
  if (.not. (extends_type_of(d4a,d4b) .and. extends_type_of(d4b,d4a))) stop 32
  if (.not. (extends_type_of(d24a,d24b) .and. extends_type_of(d24b,d24a))) stop 33
  if (.not. (extends_type_of(d343a,d343b) .and. extends_type_of(d343b,d343a))) stop 34
  if (.not. (extends_type_of(d8a,d8b) .and. extends_type_of(d8b,d8a))) stop 35
  if (.not. (extends_type_of(d28a,d28b) .and. extends_type_of(d28b,d28a))) stop 36
  if (.not. (extends_type_of(d383a,d383b) .and. extends_type_of(d383b,d383a))) stop 37

  ! arrays:
  if (.not. (same_type_as(ba,barr) .and. same_type_as(barr,ba))) stop 38
  if (.not. (same_type_as(d4a,d4arr) .and. same_type_as(d4arr,d4a))) stop 39
  if (.not. (same_type_as(d24a,d24arr) .and. same_type_as(d24arr,d24a))) stop 40
  if (.not. (same_type_as(d343a,d343arr) .and. same_type_as(d343arr,d343a))) stop 41
  if (.not. (same_type_as(d8a,d8arr) .and. same_type_as(d8arr,d8a))) stop 42
  if (.not. (same_type_as(d28a,d28arr) .and. same_type_as(d28arr,d28a))) stop 43
  if (.not. (same_type_as(d383a,d383arr) .and. same_type_as(d383arr,d383a))) stop 44

  if (.not. (extends_type_of(ba,barr) .and. extends_type_of(barr,ba))) stop 45
  if (.not. (extends_type_of(d4a,d4arr) .and. extends_type_of(d4arr,d4a))) stop 46
  if (.not. (extends_type_of(d24a,d24arr) .and. extends_type_of(d24arr,d24a))) stop 47
  if (.not. (extends_type_of(d343a,d343arr) .and. extends_type_of(d343arr,d343a))) stop 48
  if (.not. (extends_type_of(d8a,d8arr) .and. extends_type_of(d8arr,d8a))) stop 49
  if (.not. (extends_type_of(d28a,d28arr) .and. extends_type_of(d28arr,d28a))) stop 50
  if (.not. (extends_type_of(d383a,d383arr) .and. extends_type_of(d383arr,d383a))) stop 51

  ! dynamic types:
  if (.not. (same_type_as(ba,bc) .and. same_type_as(bc,ba))) stop 52
  if (.not. (extends_type_of(ba,bc) .and. extends_type_of(bc,ba))) stop 53
  allocate(Base:: bc)
  if (.not. (same_type_as(ba,bc) .and. same_type_as(bc,ba))) stop 54
  if (.not. (extends_type_of(ba,bc) .and. extends_type_of(bc,ba))) stop 55
  deallocate(bc)

  allocate(Derived(4):: bc)
  if (.not. (same_type_as(d4a,bc) .and. same_type_as(bc,d4a))) stop 56
  if (.not. (extends_type_of(bc, ba) .and. extends_type_of(bc,d4a))) stop 57
  if (.not. (extends_type_of(d24a, bc) .and. extends_type_of(d343a,bc))) stop 58

  ! try again with Derived
  allocate(Derived(4):: d4c)
  if (.not. (same_type_as(d4a,d4c) .and. same_type_as(d4c,d4a))) stop 59
  if (.not. (extends_type_of(d4c, ba) .and. extends_type_of(d4c,d4a))) stop 60
  if (.not. (extends_type_of(d24a, d4c) .and. extends_type_of(d343a,d4c))) stop 61

  ! and Derived2
  allocate(Derived2(4):: d24c)
  if (.not. (same_type_as(d24a,d24c) .and. same_type_as(d24c,d24a))) stop 62
  if (.not. (extends_type_of(d24c, ba) .and. extends_type_of(d24c,d4a))) stop 63
  if (.not. (extends_type_of(d24c, d24c) .and. extends_type_of(d343a,d24c))) stop 64

  ! and Derived3
  allocate(Derived3(4,3):: d34c)
  if (.not. (same_type_as(d343a,d34c) .and. same_type_as(d34c,d343a))) stop 65
  if (.not. (extends_type_of(d34c, ba) .and. extends_type_of(d34c,d4a) .and. extends_type_of(d34c,d24a))) stop 66
  if (.not. (extends_type_of(d34c, d343a) .and. extends_type_of(d343a,d34c))) stop 67

  ! Now a different kind:
  deallocate(bc)
  allocate(Derived(8):: bc)
  if (.not. (same_type_as(d8a,bc) .and. same_type_as(bc,d8a))) stop 68
  if (.not. (extends_type_of(bc, ba) .and. extends_type_of(bc,d8a))) stop 69
  if (.not. (extends_type_of(d28a, bc) .and. extends_type_of(d383a,bc))) stop 70

  ! try again with Derived
  allocate(Derived(8):: d8c)
  if (.not. (same_type_as(d8a,d8c) .and. same_type_as(d8c,d8a))) stop 71
  if (.not. (extends_type_of(d8c, ba) .and. extends_type_of(d8c,d8a))) stop 72
  if (.not. (extends_type_of(d28a, d8c) .and. extends_type_of(d383a,d8c))) stop 73

  ! and Derived2
  allocate(Derived2(8):: d28c)
  if (.not. (same_type_as(d28a,d28c) .and. same_type_as(d28c,d28a))) stop 74
  if (.not. (extends_type_of(d28c, ba) .and. extends_type_of(d28c,d8a))) stop 75
  if (.not. (extends_type_of(d28c, d28c) .and. extends_type_of(d383a,d28c))) stop 76

  ! and Derived3
  allocate(Derived3(8,3):: d38c)
  if (.not. (same_type_as(d383a,d38c) .and. same_type_as(d38c,d383a))) stop 77
  if (.not. (extends_type_of(d38c, ba) .and. extends_type_of(d38c,d8a) .and. extends_type_of(d38c,d28a))) stop 78
  if (.not. (extends_type_of(d38c, d383a) .and. extends_type_of(d383a,d38c))) stop 79

  print *, "done"

end program dtpUseIntrinsics01
