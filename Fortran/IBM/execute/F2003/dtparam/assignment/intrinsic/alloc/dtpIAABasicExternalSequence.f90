!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasicExternalSequence
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic sequence DTP variable (container) with allocatable components via intrinsic assignment (in external subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABasicExternal007 (<-dtpIAABasic007<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container variable with a kind and a length
!*  parameter, but with references to types which themselves have allocatable components:
!*  as Array007, but processed in an external subroutine.  Types (re)defined in
!*  external subroutines must be SEQUENCE types, which means no finalization is possible,
!*  which means we can't be sure about deallocations.
!*  We also need an interface definition.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpIAABasicExternalSequence

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

  type containera(k,l)
     integer, kind :: k
     integer, len  :: l
     sequence
     type(dk(k)), allocatable :: dkvar
     type(dl(l)), allocatable :: dlvar
  end type containera

  implicit none

  type(containera(1,:)), allocatable :: o1, o2, o3
  type(containera(4,:)), allocatable :: v1, v2, v3

  interface
     subroutine test (o1, o2, o3, v1, v2, v3)
       import :: containera
       type(containera(1,:)), allocatable :: o1, o2, o3
       type(containera(4,:)), allocatable :: v1, v2, v3
     end subroutine test
  end interface

  call test (o1, o2, o3, v1, v2, v3)

end program dtpIAABasicExternalSequence


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

  type containera(k,l)
     integer, kind :: k
     integer, len  :: l
     sequence
     type(dk(k)), allocatable :: dkvar
     type(dl(l)), allocatable :: dlvar
  end type containera
  implicit none

  type(containera(1,:)), allocatable :: o1, o2, o3
  type(containera(4,:)), allocatable :: v1, v2, v3

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  print *, allocated(o1), allocated(o2), allocated(o3)

  print *, "o1 = containera(1,2)(dk(1)(34),dl(2)('ab',[35,36]))"
  o1 = containera(1,2)(dk(1)(34),dl(2)('ab',[35,36]))

  print *, "o2 = containera(1,2)(dk(1)(37),dl(2)('cd',[38,39]))"
  o2 = containera(1,2)(dk(1)(37),dl(2)('cd',[38,39]))

  print *, "o1 = o2 {o1=", allocated(o1), o1%dkvar%ivar, o1%dlvar%chvar, o1%dlvar%iarr, &
           ", o2=", allocated(o2), o2%dkvar%ivar, o2%dlvar%chvar, o2%dlvar%iarr, "}"
  o1 = o2

  print *, "o3 = containera(1,3)(dk(1)(40),dl(3)('efg',[41,42,43]))"
  o3 = containera(1,3)(dk(1)(40),dl(3)('efg',[41,42,43]))

  print *, "o2 = o3 {o2=", allocated(o2), o2%dkvar%ivar, o2%dlvar%chvar, o2%dlvar%iarr, &
           ", o3=", allocated(o3), o3%dkvar%ivar, o3%dlvar%chvar, o3%dlvar%iarr, "}"
  o2 = o3

  ! Repeat the above with a containera of kind 4
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = containera(4,2)(dk(4)(44),dl(2)('AB',[45,46]))"
  v1 = containera(4,2)(dk(4)(44),dl(2)('AB',[45,46]))

  print *, "v2 = containera(4,2)(dk(4)(47),dl(2)('CD',[48,49]))"
  v2 = containera(4,2)(dk(4)(47),dl(2)('CD',[48,49]))

  print *, "v1 = v2 {v1=", allocated(v1), v1%dkvar%ivar, v1%dlvar%chvar, v1%dlvar%iarr, &
           ", v2=", allocated(v2), v2%dkvar%ivar, v2%dlvar%chvar, v2%dlvar%iarr, "}"
  v1 = v2

  print *, "v3 = containera(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53]))"
  v3 = containera(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53]))

  print *, "v2 = v3 {v2=", allocated(v2), v2%dkvar%ivar, v2%dlvar%chvar, v2%dlvar%iarr, &
           ", v3=", allocated(v3), v3%dkvar%ivar, v3%dlvar%chvar, v3%dlvar%iarr, "}"
  v2 = v3

  print *, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), size(o1%dlvar%iarr), o1%dkvar%ivar, o1%dlvar%chvar, o1%dlvar%iarr
  print *, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), size(o2%dlvar%iarr), o2%dkvar%ivar, o2%dlvar%chvar, o2%dlvar%iarr
  print *, allocated(o3), o3%k, o3%l, len(o3%dlvar%chvar), size(o3%dlvar%iarr), o3%dkvar%ivar, o3%dlvar%chvar, o3%dlvar%iarr

  print *, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), size(v1%dlvar%iarr), v1%dkvar%ivar, v1%dlvar%chvar, v1%dlvar%iarr
  print *, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), size(v2%dlvar%iarr), v2%dkvar%ivar, v2%dlvar%chvar, v2%dlvar%iarr
  print *, allocated(v3), v3%k, v3%l, len(v3%dlvar%chvar), size(v3%dlvar%iarr), v3%dkvar%ivar, v3%dlvar%chvar, v3%dlvar%iarr

end subroutine test
