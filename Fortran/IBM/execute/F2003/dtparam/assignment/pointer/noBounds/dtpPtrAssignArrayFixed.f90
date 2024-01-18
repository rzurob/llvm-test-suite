!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-01-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : via pointers with fixed length parameters, assign scalars to arrays in main program
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
!*  In the main program, via pointers with fixed length parameters, assign scalars to arrays
!*  of a parameterised derived type for which there is no user-defined assignment, and verify
!*  that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignArrayFixedmod

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

end module dtpPtrAssignArrayFixedmod

program dtpPtrAssignArrayFixed

  use dtpPtrAssignArrayFixedmod
  implicit none

  integer i

  type(base(3)), target      :: b_3(5)
  type(derived(3,4)), target :: d_34(3)
  type(d2(3,4,8,2)), target  :: d2_3482(1)

  type(base(5)), target      :: b_5(4)
  type(derived(5,8)), target :: d_58(2)
  type(d2(5,8,4,1)), target  :: d2_5841(1)

  type(base(3)), pointer      :: b_3p(:)
  type(derived(3,4)), pointer :: d_34p(:)
  type(d2(3,4,8,2)), pointer  :: d2_3482p(:)

  type(base(5)), pointer      :: b_5p(:)
  type(derived(5,8)), pointer :: d_58p(:)
  type(d2(5,8,4,1)), pointer  :: d2_5841p(:)


  call initValues

  b_3p => b_3
  d_34p => d_34
  d2_3482p => d2_3482
  b_5p => b_5
  d_58p => d_58
  d2_5841p => d2_5841

  b_3p = base(3)('abc')
  d_34p = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
  d2_3482p = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                        reshape([(1111_2*i,i=1,6)],[3,2]), &
                        derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

  b_5p  = base(5)('abcde')
  d_58p = derived(5,8)('defgh',1.23456789D11,.true., &
                      [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  d2_5841p = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                        [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                        reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  call checkValues

contains

  subroutine checkValues

    implicit none
    integer :: i
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8

    print *, b_3
    print *, d_34
    print *, d2_3482

    print *, b_5
    print *, d_58
    print *, d2_5841

    if (size(b_3) /= 5) error stop 22
    if (size(d_34) /= 3) error stop 23
    if (size(d2_3482) /= 1) error stop 24

    if (size(b_5) /= 4) error stop 32
    if (size(d_58) /= 2) error stop 33
    if (size(d2_5841) /= 1) error stop 34

    if (b_3%l /= 3 .or. len(b_3%ch) /= 3 .or. any(b_3%ch /= 'abc')) error stop 2

    do i = 1, 3
       if (d_34(i)%l /= 3 .or. d_34(i)%k /= 4 .or. len(d_34(i)%ch) /= 3 .or. d_34(i)%ch /= 'def' .or. .not.d_34(i)%lfld &
            .or. size(d_34(i)%ifld) /= 3 .or. kind(d_34(i)%lfld) /= 4 .or. kind(d_34(i)%ifld) /= 4 .or. kind(d_34(i)%rfld) /= 4 &
            .or. any(d_34(i)%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_34(i)%rfld,4.1_4)) then
          print *, "error in d_34(",i,")"
          stop 3
       end if
    end do

    if (d2_3482(1)%l /= 3 .or. d2_3482(1)%k /= 4 .or. d2_3482(1)%k2 /= 8 .or. d2_3482(1)%l2 /= 2 &
         .or. len(d2_3482(1)%ch) /= 3 .or. d2_3482(1)%ch /= 'ghi' .or. d2_3482(1)%lfld &
         .or. size(d2_3482(1)%ifld) /= 3 .or. kind(d2_3482(1)%lfld) /= 4 .or. kind(d2_3482(1)%ifld) /= 4 .or. kind(d2_3482(1)%rfld) /= 4 &
         .or. any(d2_3482(1)%ifld /= -d_34(1)%ifld) .or. .not.precision_r4(d2_3482(1)%rfld,5.9_4) &
         .or. kind(d2_3482(1)%iarr) /= 8 .or. any(ubound(d2_3482(1)%iarr) /= [3,2]) &
         .or. any([d2_3482(1)%iarr] /= [1111,2222,3333,4444,5555,6666])) error stop 4

    if (d2_3482(1)%der%l /= 2 .or. d2_3482(1)%der%k /= 8 .or. len(d2_3482(1)%der%ch) /= 2 &
         .or. d2_3482(1)%der%ch /= 'xz' .or. .not.d2_3482(1)%der%lfld .or. size(d2_3482(1)%der%ifld) /= 2 &
         .or. kind(d2_3482(1)%der%lfld) /= 8 .or. kind(d2_3482(1)%der%ifld) /= 8 .or. kind(d2_3482(1)%der%rfld) /= 8 &
         .or. any(d2_3482(1)%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2_3482(1)%der%rfld,11235.81321D34)) error stop 5


    if (b_5%l /= 5 .or. len(b_5%ch) /= 5 .or. any(b_5%ch /= 'abcde')) error stop 12

    do i = 1, 2
       if (d_58(i)%l /= 5 .or. d_58(i)%k /= 8 .or. len(d_58(i)%ch) /= 5 .or. d_58(i)%ch /= 'defgh' .or. .not.d_58(i)%lfld &
            .or. size(d_58(i)%ifld) /= 5 .or. kind(d_58(i)%lfld) /= 8 .or. kind(d_58(i)%ifld) /= 8 .or. kind(d_58(i)%rfld) /= 8 &
            .or. any(d_58(i)%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
            .or. .not.precision_r8(d_58(i)%rfld,1.23456789D11)) then
          print *, "error in d_58(",i,")"
          stop 13
       end if
    end do

    if (d2_5841(1)%l /= 5 .or. d2_5841(1)%k /= 8 .or. d2_5841(1)%k2 /= 4 .or. d2_5841(1)%l2 /= 1 &
         .or. len(d2_5841(1)%ch) /= 5 .or. d2_5841(1)%ch /= 'defij' .or. .not.d2_5841(1)%lfld &
         .or. size(d2_5841(1)%ifld) /= 5 .or. kind(d2_5841(1)%lfld) /= 8 .or. kind(d2_5841(1)%ifld) /= 8 .or. kind(d2_5841(1)%rfld) /= 8 &
         .or. any(d2_5841(1)%ifld /= -d_58(1)%ifld) .or. .not.precision_r8(d2_5841(1)%rfld,9.87654321D-12) &
         .or. kind(d2_5841(1)%iarr) /= 4 .or. any(ubound(d2_5841(1)%iarr) /= [5,1]) &
         .or. any([d2_5841(1)%iarr] /= [1111,2222,3333,4444,5555])) error stop 14

    if (d2_5841(1)%der%l /= 1 .or. d2_5841(1)%der%k /= 4 .or. len(d2_5841(1)%der%ch) /= 1 &
         .or. d2_5841(1)%der%ch /= 'y' .or. .not.d2_5841(1)%der%lfld .or. size(d2_5841(1)%der%ifld) /= 1 &
         .or. kind(d2_5841(1)%der%lfld) /= 4 .or. kind(d2_5841(1)%der%ifld) /= 4 .or. kind(d2_5841(1)%der%rfld) /= 4 &
         .or. any(d2_5841(1)%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2_5841(1)%der%rfld,9.87654E-12)) error stop 15

    print *, 'done'

  end subroutine checkValues

  subroutine initValues
    implicit none
    integer :: i

    b_3%ch = ''
    b_5%ch = ''

    d_34%ch = ''
    d_34%rfld = 0.0
    d_34%lfld = .false.
    do i = 1, 3
       d_34(i)%ifld = 0
    end do

    d_58%ch = ''
    d_58%rfld = 0.0
    d_58%lfld = .false.
    do i = 1, 5
       d_58(i)%ifld = 0
    end do

    d2_3482%ch = ''
    d2_3482%rfld = 0.0
    d2_3482%lfld = .false.
    d2_3482(1)%ifld = 0
    d2_3482(1)%iarr = 0
    d2_3482%der%ch = ''
    d2_3482%der%rfld = 0.0
    d2_3482%der%lfld = .false.
    d2_3482(1)%der%ifld = 0

    d2_5841%ch = ''
    d2_5841%rfld = 0.0
    d2_5841%lfld = .false.
    d2_5841(1)%ifld = 0
    d2_5841(1)%iarr = 0
    d2_5841%der%ch = ''
    d2_5841%der%rfld = 0.0
    d2_5841%der%lfld = .false.
    d2_5841(1)%der%ifld = 0

  end subroutine initValues

end program dtpPtrAssignArrayFixed
