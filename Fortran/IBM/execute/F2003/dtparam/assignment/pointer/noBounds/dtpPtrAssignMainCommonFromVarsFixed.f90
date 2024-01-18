!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignMainCommonFromVarsFixed
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : via pointers with fixed length parameter, assign variables to variables in common in main
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
!*  In the main program, use structure constructors to assign values to
!*  variables from different contexts (module, local) of a
!*  parameterised derived type for which there is no user-defined assignment,
!*  and then assign those variables to variables in a common.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignMainCommonFromVarsFixedmod

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

  type(base(3)), target      :: b_3_mod
  type(derived(3,4)), target :: d_34_mod
  type(d2(3,4,8,2)), target  :: d2_3482_mod

  type(base(5)), target      :: b_5_mod
  type(derived(5,8)), target :: d_58_mod
  type(d2(5,8,4,1)), target  :: d2_5841_mod

  type(base(3)), pointer      :: b_3_modp
  type(derived(3,4)), pointer :: d_34_modp
  type(d2(3,4,8,2)), pointer  :: d2_3482_modp

  type(base(5)), pointer      :: b_5_modp
  type(derived(5,8)), pointer :: d_58_modp
  type(d2(5,8,4,1)), pointer  :: d2_5841_modp
  
end module dtpPtrAssignMainCommonFromVarsFixedmod


program dtpPtrAssignMainCommonFromVarsFixed

  use :: dtpPtrAssignMainCommonFromVarsFixedmod

  implicit none

  type(base(3)), target      :: b_3_a
  type(derived(3,4)), target :: d_34_a
  type(d2(3,4,8,2)), target  :: d2_3482_a

  type(base(5)), target      :: b_5_a
  type(derived(5,8)), target :: d_58_a
  type(d2(5,8,4,1)), target  :: d2_5841_a

  type(base(3)), pointer      :: b_3_ap
  type(derived(3,4)), pointer :: d_34_ap
  type(d2(3,4,8,2)), pointer  :: d2_3482_ap

  type(base(5)), pointer      :: b_5_ap
  type(derived(5,8)), pointer :: d_58_ap
  type(d2(5,8,4,1)), pointer  :: d2_5841_ap

  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  integer :: i

  b_3_ap => b_3_a
  d_34_ap => d_34_a
  d2_3482_ap => d2_3482_a

  b_5_ap => b_5_a
  d_58_ap => d_58_a
  d2_5841_ap => d2_5841_a

  b_3_modp => b_3_mod
  d_34_modp => d_34_mod
  d2_3482_modp => d2_3482_mod

  b_5_modp => b_5_mod
  d_58_modp => d_58_mod
  d2_5841_modp => d2_5841_mod

  b_3_ap     = base(3)('abc')
  d_34_ap    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
  d2_3482_ap = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                           reshape([(1111_2*i,i=1,6)],[3,2]), &
                           derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

  b_5_ap     = base(5)('abcde')
  d_58_ap    = derived(5,8)('defgh',1.23456789D11,.true., &
                            [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  d2_5841_ap = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                           [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                           reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)
  b_3      = b_3_ap
  d_34     = d_34_ap
  d2_3482  = d2_3482_ap
  b_5      = b_5_ap
  d_58     = d_58_ap
  d2_5841  = d2_5841_ap
  call checkValues('internal', b_3, d_34, d2_3482, b_5, d_58, d2_5841)

  b_3_modp     = base(3)('abc')
  d_34_modp    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
  d2_3482_modp = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                             reshape([(1111_2*i,i=1,6)],[3,2]), &
                             derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

  b_5_modp     = base(5)('abcde')
  d_58_modp    = derived(5,8)('defgh',1.23456789D11,.true., &
                              [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  d2_5841_modp = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                             [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                             reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)

  b_3     = b_3_modp
  d_34    = d_34_modp
  d2_3482 = d2_3482_modp
  b_5     = b_5_modp
  d_58    = d_58_modp
  d2_5841 = d2_5841_modp

  call checkValues('module', b_3, d_34, d2_3482, b_5, d_58, d2_5841)


contains

  subroutine initValues(b_1, d_1, d2_1, b_2, d_2, d2_2)
    implicit none
    type(base(*)), intent(out)      :: b_1
    type(derived(*,4)), intent(out) :: d_1
    type(d2(*,4,8,*)), intent(out)  :: d2_1

    type(base(*)), intent(out)      :: b_2
    type(derived(*,8)), intent(out) :: d_2
    type(d2(*,8,4,*)), intent(out)  :: d2_2
    b_1  = base(3)('')
    d_1  = derived(3,4)('',0.0,.false.,0)
    d2_1 = d2(3,4,8,2)('',0.0,.false.,0,0,derived(2,8)('',0.0,.false.,0))
    b_2  = base(5)('')
    d_2  = derived(5,8)('',0.0,.false.,0)
    d2_2 = d2(5,8,4,1)('',0.0,.false.,0,0,derived(1,4)('',0.0,.false.,0))
  end subroutine initValues


  subroutine checkValues(what, b_3, d_34, d2_3482, b_5, d_58, d2_5841)
    implicit none
    character (*), intent(in) :: what

    type(base(*)), intent(in)      :: b_3
    type(derived(*,4)), intent(in) :: d_34
    type(d2(*,4,8,*)), intent(in)  :: d2_3482

    type(base(*)), intent(in)      :: b_5
    type(derived(*,8)), intent(in) :: d_58
    type(d2(*,8,4,*)), intent(in)  :: d2_5841

    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8

    print *, 'testing ', what

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

    print *, 'done ', what

  end subroutine checkValues

end program dtpPtrAssignMainCommonFromVarsFixed
