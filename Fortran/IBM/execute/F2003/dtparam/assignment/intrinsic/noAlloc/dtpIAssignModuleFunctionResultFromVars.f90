!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAssignModuleFunctionResultFromVars
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to function result in module procedures
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
!*  In internal procedures, use structure constructors to assign values to
!*  variables from different contexts (module, local) of a parameterised derived
!*  type for which there is no user-defined assignment, and then assign those
!*  variables to function results.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignModuleFunctionResultFromVarsmod

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

contains

  function baseFun1(n)
    implicit none
    integer, intent(in) :: n
    type(Base(n)) :: baseFun1
    type(Base(n)) :: tmp
    tmp = Base(n)('abc')
    baseFun1 = tmp
  end function baseFun1


  function baseFun1HA(n)
    implicit none
    integer, intent(in) :: n
    type(Base(n)) :: baseFun1HA
    type(Base(n)) :: base3_ha
    call internal
    baseFun1HA = base3_ha
  contains
    subroutine internal
      base3_ha = Base(n)('abc')
    end subroutine internal
  end function baseFun1HA


  function baseFun2(n)
    implicit none
    integer, intent(in) :: n
    type(Base(n)) :: baseFun2
    type(Base(n)) :: tmp
    tmp = Base(n)('abcde')
    baseFun2 = tmp
  end function baseFun2


  function baseFun2HA(n)
    implicit none
    integer, intent(in) :: n
    type(Base(n)) :: baseFun2HA
    type(Base(n)) :: base5_ha
    call internal
    baseFun2HA = base5_ha
  contains
    subroutine internal
      base5_ha = Base(n)('abcde')
    end subroutine internal
  end function baseFun2HA


  function derivedFun4(n)
    implicit none
    integer, intent(in) :: n
    type(Derived(n,4)) :: derivedFun4
    type(Derived(n,4)) :: tmp
    tmp = Derived(n,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    derivedFun4 = tmp
  end function derivedFun4


  function derivedFun4HA(n)
    implicit none
    integer, intent(in) :: n
    type(Derived(n,4)) :: derivedFun4HA
    type(Derived(n,4)) :: derived_34_ha
    call internal
    derivedFun4HA = derived_34_ha
  contains
    subroutine internal
      derived_34_ha = Derived(n,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
    end subroutine internal
  end function derivedFun4HA


  function derivedFun8(n)
    implicit none
    integer, intent(in) :: n
    type(Derived(n,8)) :: derivedFun8
    type(Derived(n,8)) :: tmp
    tmp = Derived(n,8)('defgh',1.23456789D11,.true., [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    derivedFun8 = tmp
  end function derivedFun8


  function derivedFun8HA(n)
    implicit none
    integer, intent(in) :: n
    type(Derived(n,8)) :: derivedFun8HA
    type(Derived(n,8)) :: derived_58_ha
    call internal
    derivedFun8HA = derived_58_ha
  contains
    subroutine internal
      derived_58_ha = Derived(n,8)('defgh',1.23456789D11,.true., [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
    end subroutine internal
  end function derivedFun8HA


  function d2Fun48(n,n2)
    implicit none
    integer, intent(in) :: n, n2
    integer :: i
    type(D2(n,4,8,n2)) :: d2Fun48
    type(D2(n,4,8,n2)) :: tmp
    tmp = D2(n,4,8,n2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                       reshape([(1111_2*i,i=1,6)],[3,2]), derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))
    d2Fun48 = tmp
  end function d2Fun48


  function d2Fun48HA(n,n2)
    implicit none
    integer, intent(in) :: n, n2
    integer :: i
    type(D2(n,4,8,n2)) :: d2Fun48HA
    type(D2(n,4,8,n2)) :: d2_3482_ha
    call internal
    d2Fun48HA = d2_3482_ha
  contains
    subroutine internal
      d2_3482_ha = D2(n,4,8,n2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                                reshape([(1111_2*i,i=1,6)],[3,2]), derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))
    end subroutine internal
  end function d2Fun48HA

  function d2Fun84(n,n2)
    implicit none
    integer, intent(in) :: n, n2
    integer :: i
    type(D2(n,8,4,n2)) :: d2Fun84
    type(D2(n,8,4,n2)) :: tmp
    tmp = D2(n,8,4,n2)('defij',9.87654321D-12,.true., [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                       reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))
    d2Fun84 = tmp
  end function d2Fun84

  function d2Fun84HA(n,n2)
    implicit none
    integer, intent(in) :: n, n2
    integer :: i
    type(D2(n,8,4,n2)) :: d2Fun84HA
    type(D2(n,8,4,n2)) :: d2_5841_ha
    call internal
    d2Fun84HA = d2_5841_ha
  contains
    subroutine internal
      d2_5841_ha  = D2(n,8,4,n2)('defij',9.87654321D-12,.true., [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                 reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))
    end subroutine internal
  end function d2Fun84HA


  subroutine testBase1(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'abc') stop 2
  end subroutine testBase1


  subroutine testBase2(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'abcde') stop 12
  end subroutine testBase2


  subroutine testDerived4(d)
    implicit none
    type(Derived(*,4)), intent(in) :: d
    logical(4) :: precision_r4
    external :: precision_r4
    print *, d
    if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'def' .or. .not.d%lfld &
         .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
         .or. any(d%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d%rfld,4.1_4)) stop 3
  end subroutine testDerived4


  subroutine testDerived8(d)
    implicit none
    type(Derived(*,8)), intent(in) :: d
    logical(4) :: precision_r8
    external :: precision_r8
    print *, d
    if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'defgh' .or. .not.d%lfld &
         .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
         .or. any(d%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
         .or. .not.precision_r8(d%rfld,1.23456789D11)) stop 13
  end subroutine testDerived8


  subroutine testD2_48(d2v)
    implicit none
    type(D2(*,4,8,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    print *, d2v
    if (d2v%l /= 3 .or. d2v%k /= 4 .or. d2v%k2 /= 8 .or. d2v%l2 /= 2 &
         .or. len(d2v%ch) /= 3 .or. d2v%ch /= 'ghi' .or. d2v%lfld &
         .or. size(d2v%ifld) /= 3 .or. kind(d2v%lfld) /= 4 .or. kind(d2v%ifld) /= 4 .or. kind(d2v%rfld) /= 4 &
         .or. any(d2v%ifld /= [-1111111111,-2122222222,-1333333333]) .or. .not.precision_r4(d2v%rfld,5.9_4) &
         .or. kind(d2v%iarr) /= 8 .or. any(ubound(d2v%iarr) /= [3,2]) &
         .or. any([d2v%iarr] /= [1111,2222,3333,4444,5555,6666])) stop 4
    if (d2v%der%l /= 2 .or. d2v%der%k /= 8 .or. len(d2v%der%ch) /= 2 &
         .or. d2v%der%ch /= 'xz' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 2 &
         .or. kind(d2v%der%lfld) /= 8 .or. kind(d2v%der%ifld) /= 8 .or. kind(d2v%der%rfld) /= 8 &
         .or. any(d2v%der%ifld /= [76543211234567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2v%der%rfld,11235.81321D34)) stop 5
  end subroutine testD2_48


  subroutine testD2_84(d2v)
    implicit none
    type(D2(*,8,4,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    print *, d2v
    if (d2v%l /= 5 .or. d2v%k /= 8 .or. d2v%k2 /= 4 .or. d2v%l2 /= 1 &
         .or. len(d2v%ch) /= 5 .or. d2v%ch /= 'defij' .or. .not.d2v%lfld &
         .or. size(d2v%ifld) /= 5 .or. kind(d2v%lfld) /= 8 .or. kind(d2v%ifld) /= 8 .or. kind(d2v%rfld) /= 8 &
         .or. any(d2v%ifld /= [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8]) &
         .or. .not.precision_r8(d2v%rfld,9.87654321D-12) &
         .or. kind(d2v%iarr) /= 4 .or. any(ubound(d2v%iarr) /= [5,1]) &
         .or. any([d2v%iarr] /= [1111,2222,3333,4444,5555])) stop 14
    if (d2v%der%l /= 1 .or. d2v%der%k /= 4 .or. len(d2v%der%ch) /= 1 &
         .or. d2v%der%ch /= 'y' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 1 &
         .or. kind(d2v%der%lfld) /= 4 .or. kind(d2v%der%ifld) /= 4 .or. kind(d2v%der%rfld) /= 4 &
         .or. any(d2v%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2v%der%rfld,9.87654E-12)) stop 15
  end subroutine testD2_84

end module dtpIAssignModuleFunctionResultFromVarsmod


program dtpIAssignModuleFunctionResultFromVars

  use :: dtpIAssignModuleFunctionResultFromVarsmod
  implicit none
  type(Base(3)) :: base3_ha
  type(Base(5)) :: base5_ha
  type(Derived(3,4)) :: derived_34_ha
  type(Derived(5,8)) :: derived_58_ha
  type(D2(3,4,8,2)) :: d2_3482_ha
  type(D2(5,8,4,1)) :: d2_5841_ha

  call testBase1(baseFun1(3))
  call testBase2(baseFun2(5))
  call testDerived4(derivedFun4(3))
  call testDerived8(derivedFun8(5))
  call testD2_48(d2Fun48(3,2))
  call testD2_84(d2Fun84(5,1))
  call testBase1(baseFun1HA(3))
  call testBase2(baseFun2HA(5))
  call testDerived4(derivedFun4HA(3))
  call testDerived8(derivedFun8HA(5))
  call testD2_48(d2Fun48HA(3,2))
  call testD2_84(d2Fun84HA(5,1))
  print *, 'done'

end program dtpIAssignModuleFunctionResultFromVars
