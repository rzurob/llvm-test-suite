!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate sequence DTP array (container) via intrinsic assignment (in external subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArrayExternal003 (<-dtpIAAArray003<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container array with a kind and a length
!*  parameter, we verify that the variables are initially unallocated, allocated after
!*  assignment, and deallocated before assignment.**
!*  Like ArrayExternal003, but with sequence types.
!*  ** - Actually, because we use SEQUENCE types, which cannot be finalized, the
!*  test for deallocation cannot be properly performed.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpIAAArrayExternalSequence

  type dk(k)
     integer, kind :: k
     sequence
     integer(k) :: ivar
  end type dk

  type dl(l)
     integer, len :: l
     sequence
     character(l) :: chvar
     integer :: iarr(l)
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     sequence
     type(dk(k)) :: dkvar
     type(dl(l)) :: dlvar
  end type container

  implicit none

  interface
     subroutine test (o1, o2, o3, v1, v2, v3)
       import :: container
       type(container(1,:)), allocatable :: o1(:), o2(:), o3(:)
       type(container(4,:)), allocatable :: v1(:), v2(:), v3(:)
     end subroutine test
  end interface

  type(container(1,:)), allocatable :: o1(:), o2(:), o3(:)
  type(container(4,:)), allocatable :: v1(:), v2(:), v3(:)

  print *, allocated(o1), allocated(o2), allocated(o3), allocated(v1), allocated(v2), allocated(v3)

  call test (o1, o2, o3, v1, v2, v3)

  print *, allocated(o1), allocated(o2), allocated(o3), allocated(v1), allocated(v2), allocated(v3)

end program dtpIAAArrayExternalSequence


subroutine test (o1, o2, o3, v1, v2, v3)

  type dk(k)
     integer, kind :: k
     sequence
     integer(k) :: ivar
  end type dk

  type dl(l)
     integer, len :: l
     sequence
     character(l) :: chvar
     integer :: iarr(l)
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     sequence
     type(dk(k)) :: dkvar
     type(dl(l)) :: dlvar
  end type container

  implicit none

  type(container(1,:)), allocatable :: o1(:), o2(:), o3(:)
  type(container(1,3)) :: o4
  type(container(4,:)), allocatable :: v1(:), v2(:), v3(:)
  type(container(4,3)) :: v4

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2

  print *, allocated(o1), allocated(o2), allocated(o3)

  print *, "o1 = [container(1,2):: container(1,2)(dk(1)(34),dl(2)('ab',[35,36]))]"
  o1 = [container(1,2):: container(1,2)(dk(1)(34),dl(2)('ab',[35,36]))]

  print *, "o2 = [container(1,2)(dk(1)(37),dl(2)('cd',[38,39]))]"
  o2 = [container(1,2)(dk(1)(37),dl(2)('cd',[38,39]))]

  print *, "o1 = o2 {o1=", allocated(o1), o1, ", o2=", allocated(o2), o2, "}"
  o1 = o2

  print *, "o3 = [container(1,3)(dk(1)(40),dl(3)('efg',[41,42,43])),container(1,3)(dk(1)(-37),dl(3)('pqr',[-38,-39,-40]))]"
  o3 = [container(1,3)(dk(1)(40),dl(3)('efg',[41,42,43])),container(1,3)(dk(1)(-37),dl(3)('pqr',[-38,-39,-40]))]

  print *, "o2 = o3 {o2=", allocated(o2), o2, ", o3=", allocated(o3), o3, "}"
  o2 = o3

  print *, "o4 = container(1,3)(dk(1)(-40),dl(3)('rst',[-41,-42,-43]))"
  o4 = container(1,3)(dk(1)(-40),dl(3)('rst',[-41,-42,-43]))

  print *, "o3 = o4 {o3=", allocated(o3), o3, ", o4=", o4, "}"
  o3 = o4

  ! Repeat the above with a container of kind 4
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = [container(4,2)(dk(4)(44),dl(2)('AB',[45,46]))]"
  v1 = [container(4,2)(dk(4)(44),dl(2)('AB',[45,46]))]

  print *, "v2 = [container(4,2)(dk(4)(47),dl(2)('CD',[48,49]))]"
  v2 = [container(4,2)(dk(4)(47),dl(2)('CD',[48,49]))]

  print *, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  v1 = v2

  print *, "v3 = [container(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53])),container(4,3)(dk(4)(-50),dl(3)('PQR',[-51,-52,-53]))]"
  v3 = [container(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53])),container(4,3)(dk(4)(-50),dl(3)('PQR',[-51,-52,-53]))]

  print *, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  v2 = v3

  print *, "v4 = container(4,3)(dk(4)(60),dl(3)('RST',[61,62,63]))"
  v4 = container(4,3)(dk(4)(60),dl(3)('RST',[61,62,63]))

  print *, "v3 = v4 {v3=", allocated(v3), v3, ", v4=", v4, "}"
  v3 = v4

  print *, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), o1
  print *, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), o2
  print *, allocated(o3), o3%k, o3%l, len(o3%dlvar%chvar), o3

  print *, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), v1
  print *, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), v2
  print *, allocated(v3), v3%k, v3%l, len(v3%dlvar%chvar), v3

end subroutine test
