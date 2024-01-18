!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignExternalCommonFromVarsFixed
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : via pointers with fixed length parameter, assign variables to variables in common in external procedure
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
!*  In external procedures, use structure constructors to assign values to
!*  variables from different contexts (host-associated, module, local) of a
!*  parameterised derived type for which there is no user-defined assignment,
!*  create pointers (in the same context) to those variables, and use the
!*  pointers to assign those variables to variables in a common.
!*  Verify that the type parameters and data values are as expected.
!*
!*  Note that in this series, only SEQUENCE types can be used, so no inheritance.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignExternalCommonFromVarsFixedmod

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
  
end module dtpPtrAssignExternalCommonFromVarsFixedmod


subroutine checkValues(what)

  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  character(*) :: what
  logical(4) :: precision_r4, precision_r8
  external :: precision_r4, precision_r8
  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

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


subroutine initValues
  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

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


subroutine localTest
  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  integer :: i
  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  type(base(3)), target      :: b_3_l1
  type(base(3)), pointer      :: b_3_l1p
  type(derived(3,4)), target :: d_34_l1
  type(derived(3,4)), pointer :: d_34_l1p
  type(d2(3,4,8,2)), target  :: d2_3482_l1
  type(d2(3,4,8,2)), pointer  :: d2_3482_l1p

  type(base(5)), target      :: b_5_l1
  type(base(5)), pointer      :: b_5_l1p
  type(derived(5,8)), target :: d_58_l1
  type(derived(5,8)), pointer :: d_58_l1p
  type(d2(5,8,4,1)), target  :: d2_5841_l1
  type(d2(5,8,4,1)), pointer  :: d2_5841_l1p

  interface
     subroutine checkValues(what)
       character(*) :: what
     end subroutine checkValues
  end interface

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  b_3_l1     = base(3)('abc')
  d_34_l1    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
  d2_3482_l1 = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                              reshape([(1111_2*i,i=1,6)],[3,2]), &
                              derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

  b_5_l1     = base(5)('abcde')
  d_58_l1    = derived(5,8)('defgh',1.23456789D11,.true., &
                      [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  d2_5841_l1 = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                              [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                              reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  call initValues
  b_3_l1p => b_3_l1
  d_34_l1p => d_34_l1
  d2_3482_l1p => d2_3482_l1
  b_5_l1p => b_5_l1
  d_58_l1p => d_58_l1
  d2_5841_l1p => d2_5841_l1

  b_3      = b_3_l1
  d_34     = d_34_l1
  d2_3482  = d2_3482_l1
  b_5      = b_5_l1
  d_58     = d_58_l1
  d2_5841  = d2_5841_l1

  call checkValues('local')

end subroutine localTest


subroutine moduleTest
  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  integer :: i
  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  interface
     subroutine checkValues(what)
       character(*) :: what
     end subroutine checkValues
  end interface

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  b_3_modp => b_3_mod
  d_34_modp => d_34_mod
  d2_3482_modp => d2_3482_mod
  b_5_modp => b_5_mod
  d_58_modp => d_58_mod
  d2_5841_modp => d2_5841_mod

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

  call initValues

  b_3     = b_3_modp
  d_34    = d_34_modp
  d2_3482 = d2_3482_modp
  b_5     = b_5_modp
  d_58    = d_58_modp
  d2_5841 = d2_5841_modp

  call checkValues('module')

end subroutine moduleTest


subroutine hostAssociatedTest
  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  integer :: i
  type(base(3)), target      :: b_3_ha
  type(derived(3,4)), target :: d_34_ha
  type(d2(3,4,8,2)), target  :: d2_3482_ha

  type(base(5)), target      :: b_5_ha
  type(derived(5,8)), target :: d_58_ha
  type(d2(5,8,4,1)), target  :: d2_5841_ha

  type(base(3)), pointer      :: b_3p
  type(derived(3,4)), pointer :: d_34p
  type(d2(3,4,8,2)), pointer  :: d2_3482p

  type(base(5)), pointer      :: b_5p
  type(derived(5,8)), pointer :: d_58p
  type(d2(5,8,4,1)), pointer  :: d2_5841p

  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  interface
     subroutine checkValues(what)
       character(*) :: what
     end subroutine checkValues
  end interface

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  b_3_ha     = base(3)('abc')
  d_34_ha    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
  d2_3482_ha = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                            reshape([(1111_2*i,i=1,6)],[3,2]), &
                            derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

  b_5_ha     = base(5)('abcde')
  d_58_ha    = derived(5,8)('defgh',1.23456789D11,.true., &
                             [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  d2_5841_ha = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                            [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                            reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

  call initValues
  call internalTest
  call checkValues('host associated')

contains

  subroutine internalTest

    b_3p => b_3_ha
    d_34p => d_34_ha
    d2_3482p => d2_3482_ha

    b_5p => b_5_ha
    d_58p => d_58_ha
    d2_5841p => d2_5841_ha

    b_3     = b_3p
    d_34    = d_34p
    d2_3482 = d2_3482p

    b_5     = b_5p
    d_58    = d_58p
    d2_5841 = d2_5841p

  end subroutine internalTest

end subroutine hostAssociatedTest


subroutine dummyArgTest(b_1, d_1, d2_1, b_2, d_2, d2_2, b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p)
  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  type(base(*)), intent(in), target      :: b_1
  type(derived(*,4)), intent(in), target :: d_1
  type(d2(*,4,8,*)), intent(in), target  :: d2_1

  type(base(*)), intent(in), target      :: b_2
  type(derived(*,8)), intent(in), target :: d_2
  type(d2(*,8,4,*)), intent(in), target  :: d2_2

  type(base(3)), intent(out), pointer      :: b_1p
  type(derived(3,4)), intent(out), pointer :: d_1p
  type(d2(3,4,8,2)), intent(out), pointer  :: d2_1p

  type(base(5)), intent(out), pointer      :: b_2p
  type(derived(5,8)), intent(out), pointer :: d_2p
  type(d2(5,8,4,1)), intent(out), pointer  :: d2_2p

  type(base(3))      :: b_3
  type(derived(3,4)) :: d_34
  type(d2(3,4,8,2))  :: d2_3482

  type(base(5))      :: b_5
  type(derived(5,8)) :: d_58
  type(d2(5,8,4,1))  :: d2_5841

  interface
     subroutine checkValues(what)
       character(*) :: what
     end subroutine checkValues
  end interface

  common /com/ b_3, b_5, d_34, d_58, d2_3482, d2_5841

  call initValues
  b_1p => b_1
  d_1p => d_1
  d2_1p => d2_1

  b_2p => b_2
  d_2p => d_2
  d2_2p => d2_2

  b_3     = b_1p
  d_34    = d_1p
  d2_3482 = d2_1p

  b_5     = b_2p
  d_58    = d_2p
  d2_5841 = d2_2p
  call checkValues('dummy')

end subroutine dummyArgTest


program dtpPtrAssignExternalCommonFromVarsFixed

  use :: dtpPtrAssignExternalCommonFromVarsFixedmod
  implicit none
  integer i
  interface
     subroutine checkValues(what)
       character(*) :: what
     end subroutine checkValues
     subroutine dummyArgTest(b_1, d_1, d2_1, b_2, d_2, d2_2, b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p)
       import :: base, derived, d2
       type(base(*)), intent(in), target      :: b_1
       type(derived(*,4)), intent(in), target :: d_1
       type(d2(*,4,8,*)), intent(in), target  :: d2_1
       type(base(*)), intent(in), target      :: b_2
       type(derived(*,8)), intent(in), target :: d_2
       type(d2(*,8,4,*)), intent(in), target  :: d2_2

       type(base(3)), intent(out), pointer      :: b_1p
       type(derived(3,4)), intent(out), pointer :: d_1p
       type(d2(3,4,8,2)), intent(out), pointer  :: d2_1p
       type(base(5)), intent(out), pointer      :: b_2p
       type(derived(5,8)), intent(out), pointer :: d_2p
       type(d2(5,8,4,1)), intent(out), pointer  :: d2_2p
     end subroutine dummyArgTest
  end interface

  type(base(3)), pointer      :: b_3p
  type(derived(3,4)), pointer :: d_34p
  type(d2(3,4,8,2)), pointer  :: d2_3482p

  type(base(5)), pointer      :: b_5p
  type(derived(5,8)), pointer :: d_58p
  type(d2(5,8,4,1)), pointer  :: d2_5841p

  type(base(3)), target      :: b_3t = base(3)('abc')
  type(derived(3,4)), target :: d_34t = derived(3,4)('def',4.1_4,.true._4,[1111111111_4,2122222222_4,1333333333_4])
  type(d2(3,4,8,2)), target  :: d2_3482t = d2(3,4,8,2)('ghi',5.9_4,.false._4,[-1111111111_4,-2122222222_4,-1333333333_4], &
                                                       reshape([(1111_8*i,i=1,6)],[3,2]), &
                                                       derived(2,8)('xz',11235.81321D34,.true._8,[76543211234567_8,-123456787654321_8]))

  type(base(5)), target      :: b_5t = base(5)('abcde')
  type(derived(5,8)), target :: d_58t = derived(5,8)('defgh',1.23456789D11,.true._8, &
                                                     [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
  type(d2(5,8,4,1)), target  :: d2_5841t = d2(5,8,4,1)('defij',9.87654321D-12,.true._8, &
                                                       [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                                       reshape([(1111_4*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true._4,[-12345678_4]))
  

  call localTest
  call hostAssociatedTest
  call moduleTest
  call dummyArgTest(b_3t, d_34t, d2_3482t, b_5t, d_58t, d2_5841t, b_3p, d_34p, d2_3482p, b_5p, d_58p, d2_5841p)

end program dtpPtrAssignExternalCommonFromVarsFixed
