!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignExternalHostAssociatedFromVarsDeferred
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to host associated variables via deferred-length pointers in an external subprogram
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
!*  In external subprograms, allocate structure constructors to assign values to
!*  variables from different contexts (host-associated, module, local) of a
!*  parameterised derived type for which there is no user-defined assignment,
!*  and then assign those variables via pointers to host-associated variables.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignExternalHostAssociatedFromVarsDeferredmod

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

  type(base(3)), target      :: b_3_mod
  type(derived(3,4)), target :: d_34_mod
  type(d2(3,4,8,2)), target  :: d2_3482_mod

  type(base(5)), target      :: b_5_mod
  type(derived(5,8)), target :: d_58_mod
  type(d2(5,8,4,1)), target  :: d2_5841_mod

  type(base(:)), pointer      :: b_3_modp, b_3_modp2
  type(derived(:,4)), pointer :: d_34_modp, d_34_modp2
  type(d2(:,4,8,:)), pointer  :: d2_3482_modp, d2_3482_modp2

  type(base(:)), pointer      :: b_5_modp, b_5_modp2
  type(derived(:,8)), pointer :: d_58_modp, d_58_modp2
  type(d2(:,8,4,:)), pointer  :: d2_5841_modp, d2_5841_modp2

end module dtpPtrAssignExternalHostAssociatedFromVarsDeferredmod


program dtpPtrAssignExternalHostAssociatedFromVarsDeferred

  implicit none
  external :: externalTest
  call externalTest

end program dtpPtrAssignExternalHostAssociatedFromVarsDeferred


subroutine externalTest

  use :: dtpPtrAssignExternalHostAssociatedFromVarsDeferredmod
  implicit none

  type(base(3))      :: b_3, b_3a, b_3_ha_p, b_3_ha_p2
  type(derived(3,4)) :: d_34, d_34a, d_34_ha_p, d_34_ha_p2
  type(d2(3,4,8,2))  :: d2_3482, d2_3482a, d2_3482_ha_p, d2_3482_ha_p2

  type(base(5))      :: b_5, b_5a, b_5_ha_p, b_5_ha_p2
  type(derived(5,8)) :: d_58, d_58a, d_58_ha_p, d_58_ha_p2
  type(d2(5,8,4,1))  :: d2_5841, d2_5841a, d2_5841_ha_p, d2_5841_ha_p2

  pointer :: b_3_ha_p, d_34_ha_p, d2_3482_ha_p, b_5_ha_p, d_58_ha_p, d2_5841_ha_p
  pointer :: b_3_ha_p2, d_34_ha_p2, d2_3482_ha_p2, b_5_ha_p2, d_58_ha_p2, d2_5841_ha_p2
  target  :: b_3, d_34, d2_3482, b_5, d_58, d2_5841
  target  :: b_3a, d_34a, d2_3482a, b_5a, d_58a, d2_5841a

  integer :: i

  call initValues
  call internalTest
  call checkValues('internal')

  call initValues
  call hostAssociatedTest
  call checkValues('host associated')

  call initValues
  call moduleTest
  call checkValues('module')

  call initValues
  call dummyArgTest(b_3a, d_34a, d2_3482a, b_5a, d_58a, d2_5841a, &
                    b_3_ha_p, d_34_ha_p, d2_3482_ha_p, b_5_ha_p, d_58_ha_p, d2_5841_ha_p, &
                    b_3_ha_p2, d_34_ha_p2, d2_3482_ha_p2, b_5_ha_p2, d_58_ha_p2, d2_5841_ha_p2)
  call checkValues('dummy')

contains

  subroutine initValues
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

  subroutine internalTest
    integer :: i
    type(base(3)), target      :: b_3_local
    type(derived(3,4)), target :: d_34_local
    type(d2(3,4,8,2)), target  :: d2_3482_local

    type(base(5)), target      :: b_5_local
    type(derived(5,8)), target :: d_58_local
    type(d2(5,8,4,1)), target  :: d2_5841_local

    type(base(:)), pointer      :: b_3_localp, b_3_localp2
    type(derived(:,4)), pointer :: d_34_localp, d_34_localp2
    type(d2(:,4,8,:)), pointer  :: d2_3482_localp, d2_3482_localp2

    type(base(:)), pointer      :: b_5_localp, b_5_localp2
    type(derived(:,8)), pointer :: d_58_localp, d_58_localp2
    type(d2(:,8,4,:)), pointer  :: d2_5841_localp, d2_5841_localp2

    b_3_localp => b_3_local
    d_34_localp => d_34_local
    d2_3482_localp => d2_3482_local

    b_5_localp => b_5_local
    d_58_localp => d_58_local
    d2_5841_localp => d2_5841_local

    b_3_localp    = base(3)('abc')
    d_34_localp   = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482_localp= d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                                reshape([(1111_2*i,i=1,6)],[3,2]), &
                                derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5_localp    = base(5)('abcde')
    d_58_localp   = derived(5,8)('defgh',1.23456789D11,.true., &
                        [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841_localp= d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                                [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

    b_3_localp2 => b_3
    d_34_localp2 => d_34
    d2_3482_localp2 => d2_3482
    b_5_localp2 => b_5
    d_58_localp2 => d_58
    d2_5841_localp2 => d2_5841

    b_3_localp2 = b_3_localp
    d_34_localp2 = d_34_localp
    d2_3482_localp2 = d2_3482_localp
    b_5_localp2 = b_5_localp
    d_58_localp2 = d_58_localp
    d2_5841_localp2 = d2_5841_localp

  end subroutine internalTest


  subroutine hostAssociatedTest
    integer :: i


    b_3_ha_p => b_3a
    d_34_ha_p => d_34a
    d2_3482_ha_p => d2_3482a
    b_5_ha_p => b_5a
    d_58_ha_p => d_58a
    d2_5841_ha_p => d2_5841a

    b_3_ha_p     = base(3)('abc')
    d_34_ha_p    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482_ha_p = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                               reshape([(1111_2*i,i=1,6)],[3,2]), &
                               derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5_ha_p     = base(5)('abcde')
    d_58_ha_p    = derived(5,8)('defgh',1.23456789D11,.true., &
                                [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841_ha_p = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                               [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                               reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

    b_3_ha_p2 => b_3
    d_34_ha_p2 => d_34
    d2_3482_ha_p2 => d2_3482
    b_5_ha_p2 => b_5
    d_58_ha_p2 => d_58
    d2_5841_ha_p2 => d2_5841

    b_3_ha_p2     = b_3a
    d_34_ha_p2    = d_34a
    d2_3482_ha_p2 = d2_3482a
    b_5_ha_p2     = b_5a
    d_58_ha_p2    = d_58a
    d2_5841_ha_p2 = d2_5841a

  end subroutine hostAssociatedTest


  subroutine moduleTest
    use :: dtpPtrAssignExternalHostAssociatedFromVarsDeferredmod
    integer :: i


    b_3_modp     => b_3_mod
    d_34_modp    => d_34_mod
    d2_3482_modp => d2_3482_mod
    b_5_modp     => b_5_mod
    d_58_modp    => d_58_mod
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

    b_3_modp2     => b_3
    d_34_modp2    => d_34
    d2_3482_modp2 => d2_3482
    b_5_modp2     => b_5
    d_58_modp2    => d_58
    d2_5841_modp2 => d2_5841

    b_3_modp2     = b_3_modp
    d_34_modp2    = d_34_modp
    d2_3482_modp2 = d2_3482_modp
    b_5_modp2     = b_5_modp
    d_58_modp2    = d_58_modp
    d2_5841_modp2 = d2_5841_modp

  end subroutine moduleTest


  subroutine dummyArgTest(b_1, d_1, d2_1, b_2, d_2, d2_2, &
                          b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p, &
                          b_1p2, d_1p2, d2_1p2, b_2p2, d_2p2, d2_2p2)
    use :: dtpPtrAssignExternalHostAssociatedFromVarsDeferredmod
    implicit none
    type(base(*))      :: b_1, b_1p, b_1p2
    type(derived(*,4)) :: d_1, d_1p, d_1p2
    type(d2(*,4,8,*))  :: d2_1, d2_1p, d2_1p2

    type(base(*))      :: b_2, b_2p, b_2p2
    type(derived(*,8)) :: d_2, d_2p, d_2p2
    type(d2(*,8,4,*))  :: d2_2, d2_2p, d2_2p2

    intent(in)  :: b_1, d_1, d2_1, b_2, d_2, d2_2
    intent(out) :: b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p, b_1p2, d_1p2, d2_1p2, b_2p2, d_2p2, d2_2p2

    target  :: b_1, d_1, d2_1, b_2, d_2, d2_2
    pointer :: b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p, b_1p2, d_1p2, d2_1p2, b_2p2, d_2p2, d2_2p2

    b_1p2  => b_1
    d_1p2  => d_1
    d2_1p2 => d2_1
    b_2p2  => b_2
    d_2p2  => d_2
    d2_2p2 => d2_2

    b_1p2  => b_3
    d_1p2  => d_34
    d2_1p2 => d2_3482
    b_2p2  => b_5
    d_2p2  => d_58
    d2_2p2 => d2_5841

    b_1p2  = b_1p
    d_1p2  = d_1p
    d2_1p2 = d2_1p
    b_2p2  = b_2p
    d_2p2  = d_2p
    d2_2p2 = d2_2p


  end subroutine dummyArgTest


  subroutine checkValues(what)
    character (*) :: what
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

end subroutine externalTest
