!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAssignLocalFromExternal
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign to local variables in external subprogram
!*
!*  REFERENCE                  : Feature Number 358785
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  In an external subprogram, assign values to local variables of a parameterised
!*  derived type for which there is no user-defined assignment, and verify that
!*  the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignLocalFromExternalmod

  type base (l)
     integer, len :: l
     character(l) :: ch
  end type base

  type, extends(base) :: derived (k)
     integer, kind :: k
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type, extends(derived) :: d2 (k2, l2)
     integer, kind :: k2
     integer, len :: l2
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

end module dtpIAssignLocalFromExternalmod


program dtpIAssignLocalFromExternal

  implicit none
  external externalTest

  call externalTest

end program dtpIAssignLocalFromExternal


subroutine externalTest

  use :: dtpIAssignLocalFromExternalmod

  implicit none

  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841
  integer i

  logical(4) :: precision_r4, precision_r8
  external :: precision_r4, precision_r8

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

  print *, b_3
  print *, d_34
  print *, d2_3482

  print *, b_5
  print *, d_58
  print *, d2_5841

  if (b_3%l /= 3 .or. len(b_3%ch) /= 3 .or. b_3%ch /= 'abc') stop 2

  if (d_34%l /= 3 .or. d_34%k /= 4 .or. len(d_34%ch) /= 3 .or. d_34%ch /= 'def' .or. .not.d_34%lfld &
       .or. size(d_34%ifld) /= 3 .or. kind(d_34%lfld) /= 4 .or. kind(d_34%ifld) /= 4 .or. kind(d_34%rfld) /= 4 &
       .or. any(d_34%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_34%rfld,4.1_4)) stop 3

  if (d2_3482%l /= 3 .or. d2_3482%k /= 4 .or. d2_3482%k2 /= 8 .or. d2_3482%l2 /= 2 &
       .or. len(d2_3482%ch) /= 3 .or. d2_3482%ch /= 'ghi' .or. d2_3482%lfld &
       .or. size(d2_3482%ifld) /= 3 .or. kind(d2_3482%lfld) /= 4 .or. kind(d2_3482%ifld) /= 4 .or. kind(d2_3482%rfld) /= 4 &
       .or. any(d2_3482%ifld /= -d_34%ifld) .or. .not.precision_r4(d2_3482%rfld,5.9_4) &
       .or. kind(d2_3482%iarr) /= 8 .or. any(ubound(d2_3482%iarr) /= [3,2]) &
       .or. any([d2_3482%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 4

  if (d2_3482%der%l /= 2 .or. d2_3482%der%k /= 8 .or. len(d2_3482%der%ch) /= 2 &
       .or. d2_3482%der%ch /= 'xz' .or. .not.d2_3482%der%lfld .or. size(d2_3482%der%ifld) /= 2 &
       .or. kind(d2_3482%der%lfld) /= 8 .or. kind(d2_3482%der%ifld) /= 8 .or. kind(d2_3482%der%rfld) /= 8 &
       .or. any(d2_3482%der%ifld /= [76543211234567_8,-123456787654321_8]) &
       .or. .not.precision_r8(d2_3482%der%rfld,11235.81321D34)) stop 5

  if (b_5%l /= 5 .or. len(b_5%ch) /= 5 .or. b_5%ch /= 'abcde') stop 12

  if (d_58%l /= 5 .or. d_58%k /= 8 .or. len(d_58%ch) /= 5 .or. d_58%ch /= 'defgh' .or. .not.d_58%lfld &
       .or. size(d_58%ifld) /= 5 .or. kind(d_58%lfld) /= 8 .or. kind(d_58%ifld) /= 8 .or. kind(d_58%rfld) /= 8 &
       .or. any(d_58%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
       .or. .not.precision_r8(d_58%rfld,1.23456789D11)) stop 13

  if (d2_5841%l /= 5 .or. d2_5841%k /= 8 .or. d2_5841%k2 /= 4 .or. d2_5841%l2 /= 1 &
       .or. len(d2_5841%ch) /= 5 .or. d2_5841%ch /= 'defij' .or. .not.d2_5841%lfld &
       .or. size(d2_5841%ifld) /= 5 .or. kind(d2_5841%lfld) /= 8 .or. kind(d2_5841%ifld) /= 8 .or. kind(d2_5841%rfld) /= 8 &
       .or. any(d2_5841%ifld /= -d_58%ifld) .or. .not.precision_r8(d2_5841%rfld,9.87654321D-12) &
       .or. kind(d2_5841%iarr) /= 4 .or. any(ubound(d2_5841%iarr) /= [5,1]) &
       .or. any([d2_5841%iarr] /= [1111,2222,3333,4444,5555])) stop 14

  if (d2_5841%der%l /= 1 .or. d2_5841%der%k /= 4 .or. len(d2_5841%der%ch) /= 1 &
       .or. d2_5841%der%ch /= 'y' .or. .not.d2_5841%der%lfld .or. size(d2_5841%der%ifld) /= 1 &
       .or. kind(d2_5841%der%lfld) /= 4 .or. kind(d2_5841%der%ifld) /= 4 .or. kind(d2_5841%der%rfld) /= 4 &
       .or. any(d2_5841%der%ifld /= [-12345678_4]) &
       .or. .not.precision_r4(d2_5841%der%rfld,9.87654E-12)) stop 15

  print *, 'done'

end subroutine externalTest
