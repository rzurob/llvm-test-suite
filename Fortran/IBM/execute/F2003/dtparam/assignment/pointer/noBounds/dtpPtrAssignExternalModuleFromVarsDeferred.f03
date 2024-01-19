!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to module variables via deferred-length pointers in external subprogram
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
!*  In external subprograms, use structure constructors to assign values to
!*  variables from different contexts (host-associated, module, local) of a
!*  parameterised derived type for which there is no user-defined assignment,
!*  and then assign those variables via pointers to module variables.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignExternalModuleFromVarsDeferredmod

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

  type(base(3)), target      :: b_3, b_3_a
  type(derived(3,4)), target :: d_34, d_34_a
  type(d2(3,4,8,2)), target  :: d2_3482, d2_3482_a

  type(base(5)), target      :: b_5, b_5_a
  type(derived(5,8)), target :: d_58, d_58_a
  type(d2(5,8,4,1)), target  :: d2_5841, d2_5841_a

  type(base(:)), pointer      :: b_3p, b_3_ap
  type(derived(:,4)), pointer :: d_34p, d_34_ap
  type(d2(:,4,8,:)), pointer  :: d2_3482p, d2_3482_ap

  type(base(:)), pointer      :: b_5p, b_5_ap
  type(derived(:,8)), pointer :: d_58p, d_58_ap
  type(d2(:,8,4,:)), pointer  :: d2_5841p, d2_5841_ap

end module dtpPtrAssignExternalModuleFromVarsDeferredmod


program dtpPtrAssignExternalModuleFromVarsDeferred
  implicit none
  external :: externalTest
  call externalTest
end program dtpPtrAssignExternalModuleFromVarsDeferred


subroutine externalTest
  use :: dtpPtrAssignExternalModuleFromVarsDeferredmod

  implicit none

  type(base(3)), target      :: b_3_ha
  type(derived(3,4)), target :: d_34_ha
  type(d2(3,4,8,2)), target  :: d2_3482_ha

  type(base(5)), target      :: b_5_ha
  type(derived(5,8)), target :: d_58_ha
  type(d2(5,8,4,1)), target  :: d2_5841_ha

  type(base(:)), pointer      :: b_3_ha_p
  type(derived(:,4)), pointer :: d_34_ha_p
  type(d2(:,4,8,:)), pointer  :: d2_3482_ha_p

  type(base(:)), pointer      :: b_5_ha_p
  type(derived(:,8)), pointer :: d_58_ha_p
  type(d2(:,8,4,:)), pointer  :: d2_5841_ha_p

  integer :: i

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)
  call internalTest
  call checkValues('internal')

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)
  call hostAssociatedTest
  call checkValues('host associated')

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)
  call moduleTest
  call checkValues('module')

  call initValues(b_3, d_34, d2_3482, b_5, d_58, d2_5841)
  call dummyArgTest(b_3_ha, d_34_ha, d2_3482_ha, b_5_ha, d_58_ha, d2_5841_ha, &
                    b_3_ha_p, d_34_ha_p, d2_3482_ha_p, b_5_ha_p, d_58_ha_p, d2_5841_ha_p)
  call checkValues('dummy')

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


  subroutine internalTest
    integer :: i
    type(base(3)), target      :: b_3_local
    type(derived(3,4)), target :: d_34_local
    type(d2(3,4,8,2)), target  :: d2_3482_local

    type(base(5)), target      :: b_5_local
    type(derived(5,8)), target :: d_58_local
    type(d2(5,8,4,1)), target  :: d2_5841_local

    type(base(:)), pointer      :: b_3_localp
    type(derived(:,4)), pointer :: d_34_localp
    type(d2(:,4,8,:)), pointer  :: d2_3482_localp

    type(base(:)), pointer      :: b_5_localp
    type(derived(:,8)), pointer :: d_58_localp
    type(d2(:,8,4,:)), pointer  :: d2_5841_localp

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

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    b_3p      = b_3_localp
    d_34p     = d_34_localp
    d2_3482p  = d2_3482_localp
    b_5p      = b_5_localp
    d_58p     = d_58_localp
    d2_5841p  = d2_5841_localp

  end subroutine internalTest


  subroutine hostAssociatedTest
    integer :: i

    b_3_ha_p => b_3_ha
    d_34_ha_p => d_34_ha
    d2_3482_ha_p => d2_3482_ha

    b_5_ha_p => b_5_ha
    d_58_ha_p => d_58_ha
    d2_5841_ha_p => d2_5841_ha

    b_3_ha_p    = base(3)('abc')
    d_34_ha_p   = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482_ha_p= d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                             reshape([(1111_2*i,i=1,6)],[3,2]), &
                             derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5_ha_p    = base(5)('abcde')
    d_58_ha_p   = derived(5,8)('defgh',1.23456789D11,.true., &
                              [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841_ha_p= d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                             [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                             reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    b_3p     = b_3_ha_p
    d_34p    = d_34_ha_p
    d2_3482p = d2_3482_ha_p
    b_5p     = b_5_ha_p
    d_58p    = d_58_ha_p
    d2_5841p = d2_5841_ha_p

  end subroutine hostAssociatedTest


  subroutine moduleTest
    use :: dtpPtrAssignExternalModuleFromVarsDeferredmod
    integer :: i

    b_3_ap => b_3_a
    d_34_ap => d_34_a
    d2_3482_ap => d2_3482_a
    b_5_ap => b_5_a
    d_58_ap => d_58_a
    d2_5841_ap => d2_5841_a

    b_3_ap    = base(3)('abc')
    d_34_ap   = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482_ap= d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                            reshape([(1111_2*i,i=1,6)],[3,2]), &
                            derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5_ap    = base(5)('abcde')
    d_58_ap   = derived(5,8)('defgh',1.23456789D11,.true., &
                             [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841_ap= d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                            [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                            reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    b_3p     = b_3_ap
    d_34p    = d_34_ap
    d2_3482p = d2_3482_ap
    b_5p     = b_5_ap
    d_58p    = d_58_ap
    d2_5841p = d2_5841_ap

  end subroutine moduleTest


  subroutine dummyArgTest(b_1, d_1, d2_1, b_2, d_2, d2_2, b_1p, d_1p, d2_1p, b_2p, d_2p, d2_2p)
    use :: dtpPtrAssignExternalModuleFromVarsDeferredmod
    implicit none

    type(base(*)), target, intent(in)      :: b_1
    type(derived(*,4)), target, intent(in) :: d_1
    type(d2(*,4,8,*)), target, intent(in)  :: d2_1

    type(base(*)), target, intent(in)      :: b_2
    type(derived(*,8)), target, intent(in) :: d_2
    type(d2(*,8,4,*)), target, intent(in)  :: d2_2

    type(base(:)), pointer, intent(out)      :: b_1p
    type(derived(:,4)), pointer, intent(out) :: d_1p
    type(d2(:,4,8,:)), pointer, intent(out)  :: d2_1p

    type(base(:)), pointer, intent(out)      :: b_2p
    type(derived(:,8)), pointer, intent(out) :: d_2p
    type(d2(:,8,4,:)), pointer, intent(out)  :: d2_2p

    b_1p => b_1
    d_1p => d_1
    d2_1p => d2_1
    b_2p => b_2
    d_2p => d_2
    d2_2p => d2_2

    b_3p => b_3
    d_34p => d_34
    d2_3482p => d2_3482
    b_5p => b_5
    d_58p => d_58
    d2_5841p => d2_5841

    b_3p     = b_1p
    d_34p    = d_1p
    d2_3482p = d2_1p
    b_5p     = b_2p
    d_58p    = d_2p
    d2_5841p = d2_2p

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

    if (b_3%l /= 3 .or. len(b_3%ch) /= 3 .or. b_3%ch /= 'abc') error stop 2

    if (d_34%l /= 3 .or. d_34%k /= 4 .or. len(d_34%ch) /= 3 .or. d_34%ch /= 'def' .or. .not.d_34%lfld &
         .or. size(d_34%ifld) /= 3 .or. kind(d_34%lfld) /= 4 .or. kind(d_34%ifld) /= 4 .or. kind(d_34%rfld) /= 4 &
         .or. any(d_34%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d_34%rfld,4.1_4)) error stop 3

    if (d2_3482%l /= 3 .or. d2_3482%k /= 4 .or. d2_3482%k2 /= 8 .or. d2_3482%l2 /= 2 &
         .or. len(d2_3482%ch) /= 3 .or. d2_3482%ch /= 'ghi' .or. d2_3482%lfld &
         .or. size(d2_3482%ifld) /= 3 .or. kind(d2_3482%lfld) /= 4 .or. kind(d2_3482%ifld) /= 4 .or. kind(d2_3482%rfld) /= 4 &
         .or. any(d2_3482%ifld /= -d_34%ifld) .or. .not.precision_r4(d2_3482%rfld,5.9_4) &
         .or. kind(d2_3482%iarr) /= 8 .or. any(ubound(d2_3482%iarr) /= [3,2]) &
         .or. any([d2_3482%iarr] /= [1111,2222,3333,4444,5555,6666])) error stop 4

    if (d2_3482%der%l /= 2 .or. d2_3482%der%k /= 8 .or. len(d2_3482%der%ch) /= 2 &
         .or. d2_3482%der%ch /= 'xz' .or. .not.d2_3482%der%lfld .or. size(d2_3482%der%ifld) /= 2 &
         .or. kind(d2_3482%der%lfld) /= 8 .or. kind(d2_3482%der%ifld) /= 8 .or. kind(d2_3482%der%rfld) /= 8 &
         .or. any(d2_3482%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2_3482%der%rfld,11235.81321D34)) error stop 5

    if (b_5%l /= 5 .or. len(b_5%ch) /= 5 .or. b_5%ch /= 'abcde') error stop 12

    if (d_58%l /= 5 .or. d_58%k /= 8 .or. len(d_58%ch) /= 5 .or. d_58%ch /= 'defgh' .or. .not.d_58%lfld &
         .or. size(d_58%ifld) /= 5 .or. kind(d_58%lfld) /= 8 .or. kind(d_58%ifld) /= 8 .or. kind(d_58%rfld) /= 8 &
         .or. any(d_58%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d_58%rfld,1.23456789D11)) error stop 13

    if (d2_5841%l /= 5 .or. d2_5841%k /= 8 .or. d2_5841%k2 /= 4 .or. d2_5841%l2 /= 1 &
         .or. len(d2_5841%ch) /= 5 .or. d2_5841%ch /= 'defij' .or. .not.d2_5841%lfld &
         .or. size(d2_5841%ifld) /= 5 .or. kind(d2_5841%lfld) /= 8 .or. kind(d2_5841%ifld) /= 8 .or. kind(d2_5841%rfld) /= 8 &
         .or. any(d2_5841%ifld /= -d_58%ifld) .or. .not.precision_r8(d2_5841%rfld,9.87654321D-12) &
         .or. kind(d2_5841%iarr) /= 4 .or. any(ubound(d2_5841%iarr) /= [5,1]) &
         .or. any([d2_5841%iarr] /= [1111,2222,3333,4444,5555])) error stop 14

    if (d2_5841%der%l /= 1 .or. d2_5841%der%k /= 4 .or. len(d2_5841%der%ch) /= 1 &
         .or. d2_5841%der%ch /= 'y' .or. .not.d2_5841%der%lfld .or. size(d2_5841%der%ifld) /= 1 &
         .or. kind(d2_5841%der%lfld) /= 4 .or. kind(d2_5841%der%ifld) /= 4 .or. kind(d2_5841%der%rfld) /= 4 &
         .or. any(d2_5841%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2_5841%der%rfld,9.87654E-12)) error stop 15

    print *, 'done ', what

  end subroutine checkValues

end subroutine externalTest
