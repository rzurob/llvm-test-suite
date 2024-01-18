!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAssignInternalLocalFromVars
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to local variables in internal subprogram
!*
!*  REFERENCE                  : Feature Number 358785
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
!*  In internal subprograms, use structure constructors to assign values to
!*  variables from different contexts (host-associated, module, local) of a
!*  parameterised derived type for which there is no user-defined assignment,
!*  and then assign those variables to local variables.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignInternalLocalFromVarsmod

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

  type(base(3))      :: b_3_mod
  type(derived(3,4)) :: d_34_mod
  type(d2(3,4,8,2))  :: d2_3482_mod

  type(base(5))      :: b_5_mod
  type(derived(5,8)) :: d_58_mod
  type(d2(5,8,4,1))  :: d2_5841_mod
  
end module dtpIAssignInternalLocalFromVarsmod


program dtpIAssignInternalLocalFromVars

  use :: dtpIAssignInternalLocalFromVarsmod

  implicit none

  type(base(3))      :: b_3_ha
  type(derived(3,4)) :: d_34_ha
  type(d2(3,4,8,2))  :: d2_3482_ha

  type(base(5))      :: b_5_ha
  type(derived(5,8)) :: d_58_ha
  type(d2(5,8,4,1))  :: d2_5841_ha

  integer :: i

  call internalTest
  call hostAssociatedTest
  call moduleTest
  call dummyArgTest(base(3)('abc'), &
                    derived(3,4)('def',4.1_4,.true._4,[1111111111_4,2122222222_4,1333333333_4]), &
                    d2(3,4,8,2)('ghi',5.9_4,.false._4,[-1111111111_4,-2122222222_4,-1333333333_4], &
                                reshape([(1111_8*i,i=1,6)],[3,2]), &
                                derived(2,8)('xz',11235.81321D34,.true._8,[76543211234567_8,-123456787654321_8])), &
                    base(5)('abcde'), &
                    derived(5,8)('defgh',1.23456789D11,.true._8, &
                                 [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]), &
                    d2(5,8,4,1)('defij',9.87654321D-12,.true._8, &
                                [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                reshape([(1111_4*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true._4,[-12345678_4])))

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
    implicit none
    integer :: i
    type(base(3))      :: b_3_l1, b_3_l0
    type(derived(3,4)) :: d_34_l1, d_34_l0
    type(d2(3,4,8,2))  :: d2_3482_l1, d2_3482_l0

    type(base(5))      :: b_5_l1, b_5_l0
    type(derived(5,8)) :: d_58_l1, d_58_l0
    type(d2(5,8,4,1))  :: d2_5841_l1, d2_5841_l0

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

    call initValues(b_3_l0, d_34_l0, d2_3482_l0, b_5_l0, d_58_l0, d2_5841_l0)
    b_3_l0      = b_3_l1
    d_34_l0     = d_34_l1
    d2_3482_l0  = d2_3482_l1
    b_5_l0      = b_5_l1
    d_58_l0     = d_58_l1
    d2_5841_l0  = d2_5841_l1

    call checkValues('internal', b_3_l0, d_34_l0, d2_3482_l0, b_5_l0, d_58_l0, d2_5841_l0)

  end subroutine internalTest


  subroutine hostAssociatedTest
    implicit none
    integer :: i
    type(base(3))      :: b_3_local
    type(derived(3,4)) :: d_34_local
    type(d2(3,4,8,2))  :: d2_3482_local

    type(base(5))      :: b_5_local
    type(derived(5,8)) :: d_58_local
    type(d2(5,8,4,1))  :: d2_5841_local

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

    call initValues(b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)
    b_3_local     = b_3_ha
    d_34_local    = d_34_ha
    d2_3482_local = d2_3482_ha
    b_5_local     = b_5_ha
    d_58_local    = d_58_ha
    d2_5841_local = d2_5841_ha

    call checkValues('host associated', b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)

  end subroutine hostAssociatedTest


  subroutine moduleTest
    use :: dtpIAssignInternalLocalFromVarsmod
    implicit none
    integer :: i
    type(base(3))      :: b_3_local
    type(derived(3,4)) :: d_34_local
    type(d2(3,4,8,2))  :: d2_3482_local

    type(base(5))      :: b_5_local
    type(derived(5,8)) :: d_58_local
    type(d2(5,8,4,1))  :: d2_5841_local

    b_3_mod     = base(3)('abc')
    d_34_mod    = derived(3,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    d2_3482_mod = d2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                              reshape([(1111_2*i,i=1,6)],[3,2]), &
                              derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))

    b_5_mod     = base(5)('abcde')
    d_58_mod    = derived(5,8)('defgh',1.23456789D11,.true., &
                               [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    d2_5841_mod = d2(5,8,4,1)('defij',9.87654321D-12,.true., &
                              [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                              reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))

    call initValues(b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)

    b_3_local     = b_3_mod
    d_34_local    = d_34_mod
    d2_3482_local = d2_3482_mod
    b_5_local     = b_5_mod
    d_58_local    = d_58_mod
    d2_5841_local = d2_5841_mod

    call checkValues('module', b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)

  end subroutine moduleTest


  subroutine dummyArgTest(b_1, d_1, d2_1, b_2, d_2, d2_2)
    use :: dtpIAssignInternalLocalFromVarsmod
    implicit none
    type(base(*)), intent(in)      :: b_1
    type(derived(*,4)), intent(in) :: d_1
    type(d2(*,4,8,*)), intent(in)  :: d2_1

    type(base(*)), intent(in)      :: b_2
    type(derived(*,8)), intent(in) :: d_2
    type(d2(*,8,4,*)), intent(in)  :: d2_2

    type(base(3))      :: b_3_local
    type(derived(3,4)) :: d_34_local
    type(d2(3,4,8,2))  :: d2_3482_local

    type(base(5))      :: b_5_local
    type(derived(5,8)) :: d_58_local
    type(d2(5,8,4,1))  :: d2_5841_local

    call initValues(b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)
    b_3_local     = b_1
    d_34_local    = d_1
    d2_3482_local = d2_1

    b_5_local     = b_2
    d_58_local    = d_2
    d2_5841_local = d2_2
    call checkValues('dummy', b_3_local, d_34_local, d2_3482_local, b_5_local, d_58_local, d2_5841_local)

  end subroutine dummyArgTest


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

end program dtpIAssignInternalLocalFromVars
