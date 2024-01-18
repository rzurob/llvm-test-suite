!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-01-26
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign to common variables in module procedures, then check values via pointers with fixed length parameters
!*
!*  REFERENCE                  : Feature Number 360669
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  In module procedures, assign values to common variables of a parameterised derived
!*  type for which there is no user-defined assignment, and via pointers with fixed
!*  length parameters, verify that the type parameters and data values are as expected.
!*
!*  Note that in this series, only SEQUENCE types can be used, so no inheritance.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

block data dtpPtrAssignCommonFromMainDeferredData

  type base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type base

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

  type(base(3)), target      :: b_3
  type(derived(3,4)), target :: d_34
  type(d2(3,4,8,2)), target  :: d2_3482

  type(base(5)), target      :: b_5
  type(derived(5,8)), target :: d_58
  type(d2(5,8,4,1)), target  :: d2_5841

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  data b_3/base(3)('')/
  data d_34/derived(3,4)('',0.0_4,.false.,[0,0,0])/
  data d2_3482/d2(3,4,8,2)('',0.0_4,.false.,[0,0,0], reshape([(0,i=1,6)],[3,2]), derived(2,8)('',0.0D0,.true.,[0_8,0_8]))/
  data b_5 /base(5)('')/
  data d_58/derived(5,8)('',0.0D0,.false., [0_8,0_8,0_8,0_8,0_8])/
  data d2_5841/d2(5,8,4,1)('',0.0D0,.false., [0_8,0_8,0_8,0_8,0_8], reshape([(0,i=1,5)],[5,1]),derived(1,4)('',0.0,.false.,[0_4]))/

end block data dtpPtrAssignCommonFromMainDeferredData


module dtpPtrAssignCommonFromModuleFixedChkmod

  type base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type base

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

contains

  subroutine checkValues

    implicit none
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    type(base(3)), target      :: b_3
    type(derived(3,4)), target :: d_34
    type(d2(3,4,8,2)), target  :: d2_3482

    type(base(5)), target      :: b_5
    type(derived(5,8)), target :: d_58
    type(d2(5,8,4,1)), target  :: d2_5841

    common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

    type(base(3)), pointer      :: b_3p
    type(derived(3,4)), pointer :: d_34p
    type(d2(3,4,8,2)), pointer  :: d2_3482p

    type(base(5)), pointer      :: b_5p
    type(derived(5,8)), pointer :: d_58p
    type(d2(5,8,4,1)), pointer  :: d2_5841p

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    print *, b_3p
    print *, d_34p
    print *, d2_3482p

    print *, b_5p
    print *, d_58p
    print *, d2_5841p

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    if (b_3p%l /= 3 .or. len(b_3p%ch) /= 3 .or. b_3p%ch /= 'abc') stop 2

    if (d_34p%l /= 3 .or. d_34p%k /= 4 .or. len(d_34p%ch) /= 3 .or. d_34p%ch /= 'def' .or. .not.d_34p%lfld &
         .or. size(d_34p%ifld) /= 3 .or. kind(d_34p%lfld) /= 4 .or. kind(d_34p%ifld) /= 4 .or. kind(d_34p%rfld) /= 4 &
         .or. any(d_34p%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_34p%rfld,4.1_4)) stop 3

    if (d2_3482p%l /= 3 .or. d2_3482p%k /= 4 .or. d2_3482p%k2 /= 8 .or. d2_3482p%l2 /= 2 &
         .or. len(d2_3482p%ch) /= 3 .or. d2_3482p%ch /= 'ghi' .or. d2_3482p%lfld &
         .or. size(d2_3482p%ifld) /= 3 .or. kind(d2_3482p%lfld) /= 4 .or. kind(d2_3482p%ifld) /= 4 .or. kind(d2_3482p%rfld) /= 4 &
         .or. any(d2_3482p%ifld /= -d_34p%ifld) .or. .not.precision_r4(d2_3482p%rfld,5.9_4) &
         .or. kind(d2_3482p%iarr) /= 8 .or. any(ubound(d2_3482p%iarr) /= [3,2]) &
         .or. any([d2_3482p%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 4

    if (d2_3482p%der%l /= 2 .or. d2_3482p%der%k /= 8 .or. len(d2_3482p%der%ch) /= 2 &
         .or. d2_3482p%der%ch /= 'xz' .or. .not.d2_3482p%der%lfld .or. size(d2_3482p%der%ifld) /= 2 &
         .or. kind(d2_3482p%der%lfld) /= 8 .or. kind(d2_3482p%der%ifld) /= 8 .or. kind(d2_3482p%der%rfld) /= 8 &
         .or. any(d2_3482p%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2_3482p%der%rfld,11235.81321D34)) stop 5

    if (b_5p%l /= 5 .or. len(b_5p%ch) /= 5 .or. b_5p%ch /= 'abcde') stop 12

    if (d_58p%l /= 5 .or. d_58p%k /= 8 .or. len(d_58p%ch) /= 5 .or. d_58p%ch /= 'defgh' .or. .not.d_58p%lfld &
         .or. size(d_58p%ifld) /= 5 .or. kind(d_58p%lfld) /= 8 .or. kind(d_58p%ifld) /= 8 .or. kind(d_58p%rfld) /= 8 &
         .or. any(d_58p%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d_58p%rfld,1.23456789D11)) stop 13

    if (d2_5841p%l /= 5 .or. d2_5841p%k /= 8 .or. d2_5841p%k2 /= 4 .or. d2_5841p%l2 /= 1 &
         .or. len(d2_5841p%ch) /= 5 .or. d2_5841p%ch /= 'defij' .or. .not.d2_5841p%lfld &
         .or. size(d2_5841p%ifld) /= 5 .or. kind(d2_5841p%lfld) /= 8 .or. kind(d2_5841p%ifld) /= 8 .or. kind(d2_5841p%rfld) /= 8 &
         .or. any(d2_5841p%ifld /= -d_58p%ifld) .or. .not.precision_r8(d2_5841p%rfld,9.87654321D-12) &
         .or. kind(d2_5841p%iarr) /= 4 .or. any(ubound(d2_5841p%iarr) /= [5,1]) &
         .or. any([d2_5841p%iarr] /= [1111,2222,3333,4444,5555])) stop 14

    if (d2_5841p%der%l /= 1 .or. d2_5841p%der%k /= 4 .or. len(d2_5841p%der%ch) /= 1 &
         .or. d2_5841p%der%ch /= 'y' .or. .not.d2_5841p%der%lfld .or. size(d2_5841p%der%ifld) /= 1 &
         .or. kind(d2_5841p%der%lfld) /= 4 .or. kind(d2_5841p%der%ifld) /= 4 .or. kind(d2_5841p%der%rfld) /= 4 &
         .or. any(d2_5841p%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2_5841p%der%rfld,9.87654E-12)) stop 15

    print *, 'done'

  end subroutine checkValues

  subroutine initValues
    implicit none

    type(base(3)), target      :: b_3
    type(derived(3,4)), target :: d_34
    type(d2(3,4,8,2)), target  :: d2_3482

    type(base(5)), target      :: b_5
    type(derived(5,8)), target :: d_58
    type(d2(5,8,4,1)), target  :: d2_5841

    common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

    b_3%ch = ''
    b_5%ch = ''

    d_34%ch = ''
    d_34%rfld = 0.0
    d_34%lfld = .false.
    d_34%ifld = 0

    d_58%ch = ''
    d_58%rfld = 0.0
    d_58%lfld = .false.
    d_58%ifld = 0

    d2_3482%ch = ''
    d2_3482%rfld = 0.0
    d2_3482%lfld = .false.
    d2_3482%ifld = 0
    d2_3482%iarr = 0
    d2_3482%der%ch = ''
    d2_3482%der%rfld = 0.0
    d2_3482%der%lfld = .false.
    d2_3482%der%ifld = 0

    d2_5841%ch = ''
    d2_5841%rfld = 0.0
    d2_5841%lfld = .false.
    d2_5841%ifld = 0
    d2_5841%iarr = 0
    d2_5841%der%ch = ''
    d2_5841%der%rfld = 0.0
    d2_5841%der%lfld = .false.
    d2_5841%der%ifld = 0

  end subroutine initValues

  subroutine assignValues
    implicit none

    type(base(3)), target      :: b_3
    type(derived(3,4)), target :: d_34
    type(d2(3,4,8,2)), target  :: d2_3482

    type(base(5)), target      :: b_5
    type(derived(5,8)), target :: d_58
    type(d2(5,8,4,1)), target  :: d2_5841

    common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841
    integer :: i

    b_3 = base(3)('abc')
    d_34 = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482 = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                           reshape([(1111_2*i,i=1,6)],[3,2]), &
                           derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5  = base(5)('abcde')
    d_58 = derived(5,8)('defgh',1.23456789D11,.true., &
                         [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841 = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                           [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                           reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  end subroutine assignValues

end module dtpPtrAssignCommonFromModuleFixedChkmod


program dtpPtrAssignCommonFromModuleFixedChk

  use dtpPtrAssignCommonFromModuleFixedChkmod
  implicit none

  call initValues
  call assignValues
  call checkValues

end program dtpPtrAssignCommonFromModuleFixedChk
