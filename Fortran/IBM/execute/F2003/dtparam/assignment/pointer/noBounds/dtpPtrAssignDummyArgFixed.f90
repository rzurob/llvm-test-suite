!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignDummyArgFixed
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : via dummy arg pointers (with fixed length parameter), assign values to variables
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
!*  In external procedures, assign values to variables via pointer dummy arguments
!*  of a parameterised derived type for which there is no user-defined assignment,
!*  and verify that the type parameters and data values are as expected.
!*  
!*  Note that in this series, only SEQUENCE types can be used, so no inheritance.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


block data dtpPtrAssignDummyArgFixedData

  type Base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type Base

  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived

  type :: D2 (l, k, k2, l2)
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
     type(Derived(l2,k2)) :: der
  end type D2

  integer :: i

  type(Base(3)), target      :: b_3l, b_3r
  type(Derived(3,4)), target :: d_34l, d_34r
  type(D2(3,4,8,2)), target  :: d2_3482l, d2_3482r

  type(Base(5)), target      :: b_5l, b_5r
  type(Derived(5,8)), target :: d_58l, d_58r
  type(D2(5,8,4,1)), target  :: d2_5841l, d2_5841r

  common /comL/ b_3l, b_5l, d_34l, d_58l, d2_3482l, d2_5841l
  common /comR/ b_3r, b_5r, d_34r, d_58r, d2_3482r, d2_5841r

  data b_3l/Base(3)('ZYX')/
  data d_34l/Derived(3,4)('WVU',1.2_4,.false.,[3,4,5])/
  data d2_3482l/D2(3,4,8,2)('TSR',6.7_4,.true.,[8,9,10], reshape([(10+i,i=1,6)],[3,2]), Derived(2,8)('QP',17.18D0,.false.,[19_8,20_8]))/
  data b_5l/Base(5)('ONMLK')/
  data d_58l/Derived(5,8)('JIHGF',21.22D0,.true., [23_8,24_8,25_8,26_8,27_8])/
  data d2_5841l/D2(5,8,4,1)('EDCBA',28.29D0,.false., [30_8,31_8,32_8,33_8,34_8], reshape([(34+i,i=1,5)],[5,1]),Derived(1,4)('z',40.41,.true.,[42_4]))/

  data b_3r/Base(3)('abc')/
  data d_34r/Derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])/
  data d2_3482r/D2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                            reshape([(1111_2*i,i=1,6)],[3,2]), &
                            Derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))/

  data b_5r/Base(5)('abcde')/
  data d_58r/Derived(5,8)('defgh',1.23456789D11,.true., &
                          [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])/
  data d2_5841r/D2(5,8,4,1)('defij',9.87654321D-12,.true., &
                            [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                            reshape([(1111_2*i,i=1,5)],[5,1]),Derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))/

end block data dtpPtrAssignDummyArgFixedData


program dtpPtrAssignDummyArgFixed

  type Base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type Base

  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived

  type :: D2 (l, k, k2, l2)
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
     type(Derived(l2,k2)) :: der
  end type D2

  implicit none
  integer i

  interface
    subroutine assignBase(left,right)
      import :: Base
      type(Base(*)), pointer, intent(in) :: left
      type(Base(*)), pointer, intent(in) :: right
    end subroutine assignBase

    subroutine assignDerived4(left,right)
      import :: Derived
      type(Derived(*,4)), pointer, intent(in) :: left
      type(Derived(*,4)), pointer, intent(in) :: right
    end subroutine assignDerived4

    subroutine assignDerived8(left,right)
      import :: Derived
      type(Derived(*,8)), pointer, intent(in) :: left
      type(Derived(*,8)), pointer, intent(in) :: right
    end subroutine assignDerived8

    subroutine assignD2_48(left,right)
      import :: D2
      type(D2(*,4,8,*)), pointer, intent(in) :: left
      type(D2(*,4,8,*)), pointer, intent(in) :: right
    end subroutine assignD2_48

    subroutine assignD2_84(left,right)
      import :: D2
      type(D2(*,8,4,*)), pointer, intent(in) :: left
      type(D2(*,8,4,*)), pointer, intent(in) :: right
    end subroutine assignD2_84
  end interface

  type(Base(3)), target      :: b_3l, b_3r
  type(Derived(3,4)), target :: d_34l, d_34r
  type(D2(3,4,8,2)), target  :: d2_3482l, d2_3482r

  type(Base(5)), target      :: b_5l, b_5r
  type(Derived(5,8)), target :: d_58l, d_58r
  type(D2(5,8,4,1)), target  :: d2_5841l, d2_5841r

  type(Base(3)), pointer      :: b_3lp, b_3rp
  type(Derived(3,4)), pointer :: d_34lp, d_34rp
  type(D2(3,4,8,2)), pointer  :: d2_3482lp, d2_3482rp

  type(Base(5)), pointer      :: b_5lp, b_5rp
  type(Derived(5,8)), pointer :: d_58lp, d_58rp
  type(D2(5,8,4,1)), pointer  :: d2_5841lp, d2_5841rp

  common /comL/ b_3l, b_5l, d_34l, d_58l, d2_3482l, d2_5841l
  common /comR/ b_3r, b_5r, d_34r, d_58r, d2_3482r, d2_5841r

  b_3lp => b_3l
  b_3rp => b_3r
  d_34lp => d_34l
  d_34rp => d_34r
  d2_3482lp => d2_3482l
  d2_3482rp => d2_3482r
  b_5lp => b_5l
  b_5rp => b_5r
  d_58lp => d_58l
  d_58rp => d_58r
  d2_5841lp => d2_5841l
  d2_5841rp => d2_5841r

  call assignBase(b_3lp, b_3rp)
  call assignBase(b_5lp, b_5rp)

  call assignDerived4(d_34lp, d_34rp)
  call assignDerived8(d_58lp, d_58rp)

  call assignD2_48(d2_3482lp, d2_3482rp)
  call assignD2_84(d2_5841lp, d2_5841rp)

  call checkValues

end program dtpPtrAssignDummyArgFixed


subroutine assignBase(left,right)
  type Base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type Base
  implicit none
  type(Base(*)), pointer, intent(in) :: left
  type(Base(*)), pointer, intent(in) :: right

  left = right

end subroutine assignBase


subroutine assignDerived4(left,right)
  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived
  implicit none
  type(Derived(*,4)), pointer, intent(in) :: left
  type(Derived(*,4)), pointer, intent(in) :: right

  left = right

end subroutine assignDerived4


subroutine assignDerived8(left,right)
  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived
  implicit none
  type(Derived(*,8)), pointer, intent(in) :: left
  type(Derived(*,8)), pointer, intent(in) :: right

  left = right

end subroutine assignDerived8


subroutine assignD2_48(left,right)
  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived
  type :: D2 (l, k, k2, l2)
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
     type(Derived(l2,k2)) :: der
  end type D2
  implicit none
  type(D2(*,4,8,*)), pointer, intent(in)            :: left
  type(D2(*,4,8,*)), pointer, intent(in) :: right

  left = right

end subroutine assignD2_48


subroutine assignD2_84(left,right)
  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived
  type :: D2 (l, k, k2, l2)
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
     type(Derived(l2,k2)) :: der
  end type D2
  implicit none
  type(D2(*,8,4,*)), pointer, intent(in)            :: left
  type(D2(*,8,4,*)), pointer, intent(in) :: right

  left = right

end subroutine assignD2_84


subroutine checkValues

  implicit none

  type Base (l)
     integer, len :: l
     sequence
     character(l) :: ch
  end type Base

  type :: Derived (l,k)
     integer, len :: l
     integer, kind :: k
     sequence
     character(l) :: ch
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived

  type :: D2 (l, k, k2, l2)
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
     type(Derived(l2,k2)) :: der
  end type D2

  logical(4) :: precision_r4, precision_r8
  external :: precision_r4, precision_r8

  type(Base(3)), target      :: b_3
  type(Derived(3,4)), target :: d_34
  type(D2(3,4,8,2)), target  :: d2_3482

  type(Base(5)), target      :: b_5
  type(Derived(5,8)), target :: d_58
  type(D2(5,8,4,1)), target  :: d2_5841

  common /comL/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

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

end subroutine checkValues
