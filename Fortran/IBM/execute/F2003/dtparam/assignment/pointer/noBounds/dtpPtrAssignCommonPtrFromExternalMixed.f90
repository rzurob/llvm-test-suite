!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignCommonPtrFromExternalMixed
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-01-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : via pointers in COMMON with mixed deferred and fixed length parameters, assign to COMMON variables in external subprograms
!*
!*  REFERENCE                  : Feature Number 360669
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
!*  In external procedures, via pointers in COMMON with mixed deferred and fixed length parameters,
!*  assign to COMMON variables of a parameterised derived type for which there is no user-defined
!*  assignment, and verify that the type parameters and data values are as expected.
!*  
!*  Note that in this series, only SEQUENCE types can be used, so no inheritance.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


block data dtpPtrAssignCommonPtrFromExternalMixedData

  type :: derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type :: d2 (l, k, k2, l2)
     integer, len :: l
     integer, kind :: k
     integer, kind :: k2
     integer, len :: l2
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

  type(d2(:,4,8,2)), pointer  :: d2_D482p
  type(d2(3,4,8,:)), pointer  :: d2_348Dp
  type(d2(:,8,4,1)), pointer  :: d2_D841p
  type(d2(5,8,4,:)), pointer  :: d2_584Dp

  common /com/ d2_D482p, d2_348Dp, d2_D841p, d2_584Dp

  type(d2(3,4,8,2)), target  :: d2_3482
  type(d2(5,8,4,1)), target  :: d2_5841

  common /tgt/ d2_3482, d2_5841

  data d2_3482/d2(3,4,8,2)('',0.0_4,.false.,[0,0,0], reshape([(0,i=1,6)],[3,2]), derived(2,8)('',0.0D0,.true.,[0_8,0_8]))/
  data d2_5841/d2(5,8,4,1)('',0.0D0,.false., [0_8,0_8,0_8,0_8,0_8], reshape([(0,i=1,5)],[5,1]),derived(1,4)('',0.0,.false.,[0_4]))/

end block data dtpPtrAssignCommonPtrFromExternalMixedData


program dtpPtrAssignCommonPtrFromExternalMixed

  implicit none
  external setPointers, assignValues, checkValues

  call setPointers
  print *, "Test 1"
  call assignValues1
  call checkValues
  print *, "Test 2"
  call assignValues2
  call checkValues
  print *, "done"

end program dtpPtrAssignCommonPtrFromExternalMixed


subroutine setPointers

  implicit none

  type :: derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type :: d2 (l, k, k2, l2)
     integer, len :: l
     integer, kind :: k
     integer, kind :: k2
     integer, len :: l2
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

  type(d2(:,4,8,2)), pointer  :: d2_D482p
  type(d2(3,4,8,:)), pointer  :: d2_348Dp
  type(d2(:,8,4,1)), pointer  :: d2_D841p
  type(d2(5,8,4,:)), pointer  :: d2_584Dp

  common /com/ d2_D482p, d2_348Dp, d2_D841p, d2_584Dp

  type(d2(3,4,8,2)), target  :: d2_3482
  type(d2(5,8,4,1)), target  :: d2_5841

  common /tgt/ d2_3482, d2_5841

  d2_D482p => d2_3482
  d2_348Dp => d2_3482
  d2_D841p => d2_5841
  d2_584Dp => d2_5841

end subroutine setPointers


subroutine assignValues1

  implicit none

  type :: derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type :: d2 (l, k, k2, l2)
     integer, len :: l
     integer, kind :: k
     integer, kind :: k2
     integer, len :: l2
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

  type(d2(:,4,8,2)), pointer  :: d2_D482p
  type(d2(3,4,8,:)), pointer  :: d2_348Dp
  type(d2(:,8,4,1)), pointer  :: d2_D841p
  type(d2(5,8,4,:)), pointer  :: d2_584Dp

  common /com/ d2_D482p, d2_348Dp, d2_D841p, d2_584Dp

  integer :: i

  d2_D482p = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                         reshape([(1111_2*i,i=1,6)],[3,2]), &
                         derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))
  d2_D841p = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                         [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                         reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

end subroutine assignValues1


subroutine assignValues2

  implicit none

  type :: derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type :: d2 (l, k, k2, l2)
     integer, len :: l
     integer, kind :: k
     integer, kind :: k2
     integer, len :: l2
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

  type(d2(:,4,8,2)), pointer  :: d2_D482p
  type(d2(3,4,8,:)), pointer  :: d2_348Dp
  type(d2(:,8,4,1)), pointer  :: d2_D841p
  type(d2(5,8,4,:)), pointer  :: d2_584Dp

  common /com/ d2_D482p, d2_348Dp, d2_D841p, d2_584Dp

  integer :: i

  d2_348Dp = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                         reshape([(1111_2*i,i=1,6)],[3,2]), &
                         derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))
  d2_584Dp = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                         [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                         reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

end subroutine assignValues2


subroutine checkValues

  implicit none

  type :: derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type :: d2 (l, k, k2, l2)
     integer, len :: l
     integer, kind :: k
     integer, kind :: k2
     integer, len :: l2
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

  logical(4) :: precision_r4, precision_r8
  external :: precision_r4, precision_r8

  type(d2(3,4,8,2)), target  :: d2_3482
  type(d2(5,8,4,1)), target  :: d2_5841

  common /tgt/ d2_3482, d2_5841

  integer i

  print *, d2_3482
  print *, d2_5841

  if (d2_3482%l /= 3 .or. d2_3482%k /= 4 .or. d2_3482%k2 /= 8 .or. d2_3482%l2 /= 2 &
       .or. len(d2_3482%ch) /= 3 .or. d2_3482%ch /= 'ghi' .or. d2_3482%lfld &
       .or. size(d2_3482%ifld) /= 3 .or. kind(d2_3482%lfld) /= 4 .or. kind(d2_3482%ifld) /= 4 .or. kind(d2_3482%rfld) /= 4 &
       .or. any(d2_3482%ifld /= [-1111111111,-2122222222,-1333333333]) .or. .not.precision_r4(d2_3482%rfld,5.9_4) &
       .or. kind(d2_3482%iarr) /= 8 .or. any(ubound(d2_3482%iarr) /= [3,2]) &
       .or. any([d2_3482%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 4

  if (d2_3482%der%l /= 2 .or. d2_3482%der%k /= 8 .or. len(d2_3482%der%ch) /= 2 &
       .or. d2_3482%der%ch /= 'xz' .or. .not.d2_3482%der%lfld .or. size(d2_3482%der%ifld) /= 2 &
       .or. kind(d2_3482%der%lfld) /= 8 .or. kind(d2_3482%der%ifld) /= 8 .or. kind(d2_3482%der%rfld) /= 8 &
       .or. any(d2_3482%der%ifld /= [76543211234567_8,-123456787654321_8]) &
       .or. .not.precision_r8(d2_3482%der%rfld,11235.81321D34)) stop 5

  if (d2_5841%l /= 5 .or. d2_5841%k /= 8 .or. d2_5841%k2 /= 4 .or. d2_5841%l2 /= 1 &
       .or. len(d2_5841%ch) /= 5 .or. d2_5841%ch /= 'defij' .or. .not.d2_5841%lfld &
       .or. size(d2_5841%ifld) /= 5 .or. kind(d2_5841%lfld) /= 8 .or. kind(d2_5841%ifld) /= 8 .or. kind(d2_5841%rfld) /= 8 &
       .or. any(d2_5841%ifld /= [(-1111111111111_8*i,i=1,5)]) .or. .not.precision_r8(d2_5841%rfld,9.87654321D-12) &
       .or. kind(d2_5841%iarr) /= 4 .or. any(ubound(d2_5841%iarr) /= [5,1]) &
       .or. any([d2_5841%iarr] /= [1111,2222,3333,4444,5555])) stop 14

  if (d2_5841%der%l /= 1 .or. d2_5841%der%k /= 4 .or. len(d2_5841%der%ch) /= 1 &
       .or. d2_5841%der%ch /= 'y' .or. .not.d2_5841%der%lfld .or. size(d2_5841%der%ifld) /= 1 &
       .or. kind(d2_5841%der%lfld) /= 4 .or. kind(d2_5841%der%ifld) /= 4 .or. kind(d2_5841%der%rfld) /= 4 &
       .or. any(d2_5841%der%ifld /= [-12345678_4]) &
       .or. .not.precision_r4(d2_5841%der%rfld,9.87654E-12)) stop 15

end subroutine checkValues
