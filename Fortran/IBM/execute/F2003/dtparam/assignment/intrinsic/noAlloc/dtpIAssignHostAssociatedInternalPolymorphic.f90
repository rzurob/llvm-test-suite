!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign from host-associated polymorphic variables in main program
!*
!*  REFERENCE                  : Feature Number 358785
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : module, save
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  In internal subroutines, assign values to variables local to the main program
!*  of a parameterised derived type for which there is no user-defined assignment
!*  and which are declared as targets.  Assign references to them to polymorpic
!*  pointers, and use the pointers on the RHS of an assignment statement.
!*  Verify that the type parameters and data values of both pointer and variable
!*  (LHS of second assignment) are as expected.
!*  Note that the polymorphic variables do not appear on the LHS of any assignment
!*  statement - we merely test the reference.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignHostAssociatedInternalPolymorphicmod

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

end module dtpIAssignHostAssociatedInternalPolymorphicmod

program dtpIAssignHostAssociatedInternalPolymorphic

  use dtpIAssignHostAssociatedInternalPolymorphicmod
  implicit none

  integer i

  type(base(3)), target      :: b_3, b_3a
  type(derived(3,4)), target :: d_34, d_34a
  type(derived(1,4)), target :: d_14a
  type(d2(3,4,8,2)), target  :: d2_3482, d2_3482a

  type(base(5)), target      :: b_5, b_5a
  type(derived(5,8)), target :: d_58, d_58a, d_x8a
  type(derived(2,8)), target :: d_28a
  type(d2(5,8,4,1)), target  :: d2_5841, d2_5841a

  class(base(:)), pointer      :: b_xp, b_xap, b_xbp
  class(derived(:,4)), pointer :: d_x4p, d_y4p
  class(d2(:,4,8,:)), pointer  :: d2_x48yp

  class(base(:)), pointer      :: b_yp, b_yap, b_ybp
  class(derived(:,8)), pointer :: d_x8p, d_y8p
  class(d2(:,8,4,:)), pointer  :: d2_x84yp


  call initValues

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

  call internalTest

  print *, 'done'

contains

  subroutine internalTest
    ! Phase 1: pointer assignment to exact same type
    b_xp     => b_3
    d_y4p    => d_34
    d2_x48yp => d2_3482
    d_x8p    => d2_3482%der
    b_yp     => b_5
    d_y8p    => d_58
    d2_x84yp => d2_5841
    d_x4p    => d2_5841%der

    call checkValues1

    ! Phase 2: use pointers on RHS of assignment statements
    b_3a     = b_xp
    d_34a    = d_y4p
    d2_3482a = d2_x48yp
    d_28a    = d_x8p
    b_5a     = b_yp
    d_58a    = d_y8p
    d2_5841a = d2_x84yp
    d_14a    = d_x4p

    call checkValues2

    ! Phase 3: let pointer reference a child

    b_xp  => d_34
    b_xap => d2_3482
    b_xbp => d2_3482%der
    d_y4p => d2_3482

    b_yp  => d_58
    b_yap => d2_5841
    b_ybp => d2_5841%der
    d_x8p => d2_5841

    call checkValues3
  end subroutine internalTest


  subroutine checkValues1

    implicit none
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8

    print *, 'Phase 1: same type'

    if (b_xp%l /= 3 .or. len(b_xp%ch) /= 3 .or. b_xp%ch /= 'abc') stop 2

    if (d_y4p%l /= 3 .or. d_y4p%k /= 4 .or. len(d_y4p%ch) /= 3 .or. d_y4p%ch /= 'def' .or. .not.d_y4p%lfld &
         .or. size(d_y4p%ifld) /= 3 .or. kind(d_y4p%lfld) /= 4 .or. kind(d_y4p%ifld) /= 4 .or. kind(d_y4p%rfld) /= 4 &
         .or. any(d_y4p%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_y4p%rfld,4.1_4)) stop 3

    if (d_x4p%l /= 1 .or. d_x4p%k /= 4 .or. len(d_x4p%ch) /= 1 .or. d_x4p%ch /= 'y' .or. .not.d_x4p%lfld  &
         .or. size(d_x4p%ifld) /= 1 .or. kind(d_x4p%lfld) /= 4 .or. kind(d_x4p%ifld) /= 4 .or. kind(d_x4p%rfld) /= 4 &
         .or. any(d_x4p%ifld /= [-12345678_4]) .or. .not.precision_r4(d_x4p%rfld,9.87654E-12)) stop 4

    if (d2_x48yp%l /= 3 .or. d2_x48yp%k /= 4 .or. d2_x48yp%k2 /= 8 .or. d2_x48yp%l2 /= 2 &
         .or. len(d2_x48yp%ch) /= 3 .or. d2_x48yp%ch /= 'ghi' .or. d2_x48yp%lfld &
         .or. size(d2_x48yp%ifld) /= 3 .or. kind(d2_x48yp%lfld) /= 4 .or. kind(d2_x48yp%ifld) /= 4 .or. kind(d2_x48yp%rfld) /= 4 &
         .or. any(d2_x48yp%ifld /= -d_y4p%ifld) .or. .not.precision_r4(d2_x48yp%rfld,5.9_4) &
         .or. kind(d2_x48yp%iarr) /= 8 .or. any(ubound(d2_x48yp%iarr) /= [3,2]) &
         .or. any([d2_x48yp%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 5

    if (d2_x48yp%der%l /= 2 .or. d2_x48yp%der%k /= 8 .or. len(d2_x48yp%der%ch) /= 2 &
         .or. d2_x48yp%der%ch /= 'xz' .or. .not.d2_x48yp%der%lfld .or. size(d2_x48yp%der%ifld) /= 2 &
         .or. kind(d2_x48yp%der%lfld) /= 8 .or. kind(d2_x48yp%der%ifld) /= 8 .or. kind(d2_x48yp%der%rfld) /= 8 &
         .or. any(d2_x48yp%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2_x48yp%der%rfld,11235.81321D34)) stop 6


    if (b_yp%l /= 5 .or. len(b_yp%ch) /= 5 .or. b_yp%ch /= 'abcde') stop 7

    if (d_y8p%l /= 5 .or. d_y8p%k /= 8 .or. len(d_y8p%ch) /= 5 .or. d_y8p%ch /= 'defgh' .or. .not.d_y8p%lfld &
         .or. size(d_y8p%ifld) /= 5 .or. kind(d_y8p%lfld) /= 8 .or. kind(d_y8p%ifld) /= 8 .or. kind(d_y8p%rfld) /= 8 &
         .or. any(d_y8p%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d_y8p%rfld,1.23456789D11)) stop 8

    if (d_x8p%l /= 2 .or. d_x8p%k /= 8 .or. len(d_x8p%ch) /= 2 .or. d_x8p%ch /= 'xz' .or. .not.d_x8p%lfld &
         .or. size(d_x8p%ifld) /= 2 .or. kind(d_x8p%lfld) /= 8 .or. kind(d_x8p%ifld) /= 8 .or. kind(d_x8p%rfld) /= 8 &
         .or. any(d_x8p%ifld /= [76543211234567_8,-123456787654321_8]) .or. .not.precision_r8(d_x8p%rfld,11235.81321D34)) stop 9

    if (d2_x84yp%l /= 5 .or. d2_x84yp%k /= 8 .or. d2_x84yp%k2 /= 4 .or. d2_x84yp%l2 /= 1 &
         .or. len(d2_x84yp%ch) /= 5 .or. d2_x84yp%ch /= 'defij' .or. .not.d2_x84yp%lfld &
         .or. size(d2_x84yp%ifld) /= 5 .or. kind(d2_x84yp%lfld) /= 8 .or. kind(d2_x84yp%ifld) /= 8 .or. kind(d2_x84yp%rfld) /= 8 &
         .or. any(d2_x84yp%ifld /= -d_y8p%ifld) .or. .not.precision_r8(d2_x84yp%rfld,9.87654321D-12) &
         .or. kind(d2_x84yp%iarr) /= 4 .or. any(ubound(d2_x84yp%iarr) /= [5,1]) &
         .or. any([d2_x84yp%iarr] /= [1111,2222,3333,4444,5555])) stop 10

    if (d2_x84yp%der%l /= 1 .or. d2_x84yp%der%k /= 4 .or. len(d2_x84yp%der%ch) /= 1 &
         .or. d2_x84yp%der%ch /= 'y' .or. .not.d2_x84yp%der%lfld .or. size(d2_x84yp%der%ifld) /= 1 &
         .or. kind(d2_x84yp%der%lfld) /= 4 .or. kind(d2_x84yp%der%ifld) /= 4 .or. kind(d2_x84yp%der%rfld) /= 4 &
         .or. any(d2_x84yp%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2_x84yp%der%rfld,9.87654E-12)) stop 11

    print *, 'Phase 1 done'

  end subroutine checkValues1

  subroutine checkValues2

    implicit none
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8

    print *, 'Phase 2: pointer on RHS'

    if (b_3a%l /= 3 .or. len(b_3a%ch) /= 3 .or. b_3a%ch /= 'abc') stop 12

    if (d_34a%l /= 3 .or. d_34a%k /= 4 .or. len(d_34a%ch) /= 3 .or. d_34a%ch /= 'def' .or. .not.d_34a%lfld &
         .or. size(d_34a%ifld) /= 3 .or. kind(d_34a%lfld) /= 4 .or. kind(d_34a%ifld) /= 4 .or. kind(d_34a%rfld) /= 4 &
         .or. any(d_34a%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_34a%rfld,4.1_4)) stop 13

    if (d_14a%l /= 1 .or. d_14a%k /= 4 .or. len(d_14a%ch) /= 1 &
         .or. d_14a%ch /= 'y' .or. .not.d_14a%lfld .or. size(d_14a%ifld) /= 1 &
         .or. kind(d_14a%lfld) /= 4 .or. kind(d_14a%ifld) /= 4 .or. kind(d_14a%rfld) /= 4 &
         .or. any(d_14a%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d_14a%rfld,9.87654E-12)) stop 14

    if (d2_3482a%l /= 3 .or. d2_3482a%k /= 4 .or. d2_3482a%k2 /= 8 .or. d2_3482a%l2 /= 2 &
         .or. len(d2_3482a%ch) /= 3 .or. d2_3482a%ch /= 'ghi' .or. d2_3482a%lfld &
         .or. size(d2_3482a%ifld) /= 3 .or. kind(d2_3482a%lfld) /= 4 .or. kind(d2_3482a%ifld) /= 4 .or. kind(d2_3482a%rfld) /= 4 &
         .or. any(d2_3482a%ifld /= -d_34a%ifld) .or. .not.precision_r4(d2_3482a%rfld,5.9_4) &
         .or. kind(d2_3482a%iarr) /= 8 .or. any(ubound(d2_3482a%iarr) /= [3,2]) &
         .or. any([d2_3482a%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 15

    if (d2_3482a%der%l /= 2 .or. d2_3482a%der%k /= 8 .or. len(d2_3482a%der%ch) /= 2 &
         .or. d2_3482a%der%ch /= 'xz' .or. .not.d2_3482a%der%lfld .or. size(d2_3482a%der%ifld) /= 2 &
         .or. kind(d2_3482a%der%lfld) /= 8 .or. kind(d2_3482a%der%ifld) /= 8 .or. kind(d2_3482a%der%rfld) /= 8 &
         .or. any(d2_3482a%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2_3482a%der%rfld,11235.81321D34)) stop 16


    if (b_5a%l /= 5 .or. len(b_5a%ch) /= 5 .or. b_5a%ch /= 'abcde') stop 17

    if (d_58a%l /= 5 .or. d_58a%k /= 8 .or. len(d_58a%ch) /= 5 .or. d_58a%ch /= 'defgh' .or. .not.d_58a%lfld &
         .or. size(d_58a%ifld) /= 5 .or. kind(d_58a%lfld) /= 8 .or. kind(d_58a%ifld) /= 8 .or. kind(d_58a%rfld) /= 8 &
         .or. any(d_58a%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d_58a%rfld,1.23456789D11)) stop 18

    if (d_28a%l /= 2 .or. d_28a%k /= 8 .or. len(d_28a%ch) /= 2 &
         .or. d_28a%ch /= 'xz' .or. .not.d_28a%lfld .or. size(d_28a%ifld) /= 2 &
         .or. kind(d_28a%lfld) /= 8 .or. kind(d_28a%ifld) /= 8 .or. kind(d_28a%rfld) /= 8 &
         .or. any(d_28a%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d_28a%rfld,11235.81321D34)) stop 19

    if (d2_5841a%l /= 5 .or. d2_5841a%k /= 8 .or. d2_5841a%k2 /= 4 .or. d2_5841a%l2 /= 1 &
         .or. len(d2_5841a%ch) /= 5 .or. d2_5841a%ch /= 'defij' .or. .not.d2_5841a%lfld &
         .or. size(d2_5841a%ifld) /= 5 .or. kind(d2_5841a%lfld) /= 8 .or. kind(d2_5841a%ifld) /= 8 .or. kind(d2_5841a%rfld) /= 8 &
         .or. any(d2_5841a%ifld /= -d_58a%ifld) .or. .not.precision_r8(d2_5841a%rfld,9.87654321D-12) &
         .or. kind(d2_5841a%iarr) /= 4 .or. any(ubound(d2_5841a%iarr) /= [5,1]) &
         .or. any([d2_5841a%iarr] /= [1111,2222,3333,4444,5555])) stop 20

    if (d2_5841a%der%l /= 1 .or. d2_5841a%der%k /= 4 .or. len(d2_5841a%der%ch) /= 1 &
         .or. d2_5841a%der%ch /= 'y' .or. .not.d2_5841a%der%lfld .or. size(d2_5841a%der%ifld) /= 1 &
         .or. kind(d2_5841a%der%lfld) /= 4 .or. kind(d2_5841a%der%ifld) /= 4 .or. kind(d2_5841a%der%rfld) /= 4 &
         .or. any(d2_5841a%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2_5841a%der%rfld,9.87654E-12)) stop 21

    print *, 'Phase 2 done'

  end subroutine checkValues2


  subroutine checkValues3

    implicit none
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8

    print *, 'Phase 3: pointer referencing child type'

    if (b_xp%l /= 3 .or. len(b_xp%ch) /= 3 .or. b_xp%ch /= 'def') stop 22
    if (b_xap%l /= 3 .or. len(b_xap%ch) /= 3 .or. b_xap%ch /= 'ghi') stop 23
    if (b_xbp%l /= 2 .or. len(b_xbp%ch) /= 2 .or. b_xbp%ch /= 'xz') stop 24

    if (d_y4p%l /= 3 .or. d_y4p%k /= 4 .or. len(d_y4p%ch) /= 3 .or. d_y4p%ch /= 'ghi' .or. d_y4p%lfld &
         .or. size(d_y4p%ifld) /= 3 .or. kind(d_y4p%lfld) /= 4 .or. kind(d_y4p%ifld) /= 4 .or. kind(d_y4p%rfld) /= 4 &
         .or. any(d_y4p%ifld /= [-1111111111,-2122222222,-1333333333]) .or. .not.precision_r4(d_y4p%rfld,5.9_4)) stop 25

    if (b_yp%l /= 5 .or. len(b_yp%ch) /= 5 .or. b_yp%ch /= 'defgh') stop 26
    if (b_yap%l /= 5 .or. len(b_yap%ch) /= 5 .or. b_yap%ch /= 'defij') stop 27
    if (b_ybp%l /= 1 .or. len(b_ybp%ch) /= 1 .or. b_ybp%ch /= 'y') stop 28

    if (d_x8p%l /= 5 .or. d_x8p%k /= 8 .or. len(d_x8p%ch) /= 5 .or. d_x8p%ch /= 'defij' .or. .not.d_x8p%lfld &
         .or. size(d_x8p%ifld) /= 5 .or. kind(d_x8p%lfld) /= 8 .or. kind(d_x8p%ifld) /= 8 .or. kind(d_x8p%rfld) /= 8 &
         .or. any(d_x8p%ifld /= -[1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d_x8p%rfld,9.87654321D-12)) stop 29

    print *, 'Phase 3 done'

  end subroutine checkValues3


  subroutine initValues
    implicit none

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

end program dtpIAssignHostAssociatedInternalPolymorphic
