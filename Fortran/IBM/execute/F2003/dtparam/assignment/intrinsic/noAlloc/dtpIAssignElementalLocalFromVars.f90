!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAssignElementalLocalFromVars
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to local variables in elemental procedure
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
!*  In elemental module procedures, assign structures to local variables and return values
!*  of a parameterised derived type for which there is no user-defined assignment.
!*  Verify that the type parameters and data values are as expected.
!*  Note from JX: 2009-12-28: elemental functions can NOT return a result with
!   a length type parameter being non-constant.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignElementalLocalFromVarsmod
  use ieee_arithmetic

  type KBase (k)
     integer, kind :: k
     integer(k) :: ival
  end type KBase

  type Base (l)
     integer, len :: l
     character(l) :: ch
  end type Base

  type, extends(Base) :: Derived (k)
     integer, kind :: k
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type Derived

  type, extends(Derived) :: D2 (k2, l2)
     integer, kind :: k2
     integer, len :: l2
     integer(k2) :: iArr(l,l2)
     type(Derived(l2,k2)) :: der
  end type D2

  type IArrWrapper(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: iArr(l)
  end type IArrWrapper

contains

  ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal4(value,expected)
    real(4), intent(in) :: value, expected
    real(4) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal4 = .true.
    else
       delta = expected * 0.00001
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0E0) then
          isSameReal4 = ((value >= high) .and. (value <= low))
       else
          isSameReal4 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal4


  ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal8(value,expected)
    real(8), intent(in) :: value, expected
    real(8) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal8 = .true.
    else
       delta = expected * 0.00000000000001D0
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0D0) then
          isSameReal8 = ((value >= high) .and. (value <= low))
       else
          isSameReal8 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal8


  elemental integer function chkKBase4(bv, intExp)
    integer, parameter :: KEXP = 4
    class(KBase(KEXP)), intent(in) :: bv
    integer(KEXP), intent(in) :: intExp
    chkKBase4 = 0
    if (bv%k /= KEXP .or. kind(bv%ival) /= KEXP .or. bv%ival /= intExp) chkKBase4 = 2
  end function chkKBase4

  elemental integer function chkLocalKBase4(intVal)
    integer, parameter :: KEXP = 4
    integer(KEXP), intent(in) :: intVal
    type(KBase(KEXP)) :: tmp
    tmp = KBase(KEXP)(intVal)
    chkLocalKBase4 = chkKBase4(tmp, intVal)
  end function chkLocalKBase4

  elemental function map2KBase4(i)
    integer, parameter :: KEXP = 4
    integer(KEXP), intent(in) :: i
    type(KBase(KEXP)) :: map2KBase4
    map2KBase4 = KBase(KEXP)(i)
  end function map2KBase4


  elemental integer function chkKBase8(bv, intExp)
    integer, parameter :: KEXP = 8
    class(KBase(KEXP)), intent(in) :: bv
    integer(KEXP), intent(in) :: intExp
    chkKBase8 = 0
    if (bv%k /= KEXP .or. kind(bv%ival) /= KEXP .or. bv%ival /= intExp) chkKBase8 = 2
  end function chkKBase8

  elemental integer function chkLocalKBase8(intVal)
    integer, parameter :: KEXP = 8
    integer(KEXP), intent(in) :: intVal
    type(KBase(KEXP)) :: tmp
    tmp = KBase(KEXP)(intVal)
    chkLocalKBase8 = chkKBase8(tmp, intVal)
  end function chkLocalKBase8

  elemental function map2KBase8(i)
    integer, parameter :: KEXP = 8
    integer(KEXP), intent(in) :: i
    type(KBase(KEXP)) :: map2KBase8
    map2KBase8 = KBase(KEXP)(i)
  end function map2KBase8


  elemental integer function chkBase(bv, chExp, lenExp)
    class(Base(*)), intent(in) :: bv
    character(*), intent(in) :: chExp
    integer, intent(in) :: lenExp
    character(2) :: ch1, ch2
    character(1) :: ch(4)
    integer(4) :: ians

    chkBase = 0
    if (bv%l /= lenExp .or. len(bv%ch) /= lenExp .or. bv%ch /= chExp) chkBase = 2
  end function chkBase

  elemental integer function chkLocalBase(chVal,lenExp)
    character(*), intent(in) :: chVal
    integer, intent(in) :: lenExp
    type(Base(len(chVal))) :: tmp
    tmp = Base(len(chVal))(chVal)
    chkLocalBase = chkBase(tmp, chVal, lenExp)
  end function chkLocalBase

  elemental function map2Base(ch)
    character(*), intent(in) :: ch
    type(Base(3)) :: map2Base
    map2Base = Base(3)(ch)
  end function map2Base


  elemental integer function chkDerived4(dv, chExp, rExp, lExp, iExp, lenExp)
    integer, parameter :: KEXP = 4
    class(Derived(*,KEXP)), intent(in) :: dv
    character(*), intent(in) :: chExp
    real(KEXP), intent(in) :: rExp
    logical(KEXP), intent(in) :: lExp
    type(IArrWrapper(KEXP,*)), intent(in) :: iExp
    integer, intent(in) :: lenExp

    chkDerived4 = chkBase(dv%Base, chExp, lenExp)
    if (dv%l /= lenExp .or. dv%k /= KEXP) chkDerived4 = chkDerived4 + 8
    if (dv%lfld .neqv. lExp .or. kind(dv%lfld) /= KEXP) chkDerived4 = chkDerived4 + 16
    if (size(dv%ifld) /= lenExp .or. kind(dv%ifld) /= KEXP .or. any(dv%ifld /= iExp%iArr)) chkDerived4 = chkDerived4 + 32
    if (kind(dv%rfld) /= KEXP .or. .not.isSameReal4(dv%rfld,rExp)) chkDerived4 = chkDerived4 + 64
  end function chkDerived4

  elemental integer function chkDerivedDerived4(dv, dvExp)
    integer, parameter :: KEXP  = 4
    class(Derived(*,KEXP)), intent(in) :: dv, dvExp
    chkDerivedDerived4 = chkBase(dv%Base, dvExp%ch, dvExp%l)
    if (dv%l /= dvExp%l .or. dv%k /= KEXP) chkDerivedDerived4 = chkDerivedDerived4 + 8
    if (dv%lfld .neqv. dvExp%lfld .or. kind(dv%lfld) /= KEXP) chkDerivedDerived4 = chkDerivedDerived4 + 16
    if (size(dv%ifld) /= dvExp%l .or. kind(dv%ifld) /= KEXP .or. any(dv%ifld /= dvExp%ifld)) chkDerivedDerived4 = chkDerivedDerived4 + 32
    if (kind(dv%rfld) /= KEXP .or. .not.isSameReal4(dv%rfld,dvExp%rfld)) chkDerivedDerived4 = chkDerivedDerived4 + 64
  end function chkDerivedDerived4

  elemental integer function chkLocalDerived4(chVal, rVal, lVal, iVal, lenVal)
    integer, parameter :: KEXP = 4

    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal

    type(Derived(len(chVal),KEXP)) :: tmp

    tmp = Derived(len(chVal),KEXP)(chVal,rVal,lVal,iVal%iArr)
    chkLocalDerived4 = chkDerived4(tmp, chVal, rVal, lVal, iVal, lenVal)
  end function chkLocalDerived4

  elemental function map2Derived4(chVal, rVal, lVal, iVal, lenVal)
    integer, parameter :: KEXP = 4
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal
    type(Derived(2,KEXP)) :: map2Derived4
    map2Derived4 = Derived(2,KEXP)(chVal,rVal,lVal,iVal%iArr)
  end function map2Derived4


  elemental integer function chkDerived8(dv, chExp, rExp, lExp, iExp, lenExp)
    integer, parameter :: KEXP = 8
    class(Derived(*,KEXP)), intent(in) :: dv
    character(*), intent(in) :: chExp
    real(KEXP), intent(in) :: rExp
    logical(KEXP), intent(in) :: lExp
    type(IArrWrapper(KEXP,*)), intent(in) :: iExp
    integer, intent(in) :: lenExp

    chkDerived8 = chkBase(dv%Base, chExp, lenExp)
    if (dv%l /= lenExp .or. dv%k /= KEXP) chkDerived8 = chkDerived8 + 8
    if (dv%lfld .neqv. lExp .or. kind(dv%lfld) /= KEXP) chkDerived8 = chkDerived8 + 16
    if (size(dv%ifld) /= lenExp .or. kind(dv%ifld) /= KEXP .or. any(dv%ifld /= iExp%iArr)) chkDerived8 = chkDerived8 + 32
    if (kind(dv%rfld) /= KEXP .or. .not.isSameReal8(dv%rfld,rExp)) chkDerived8 = chkDerived8 + 64
  end function chkDerived8

  elemental integer function chkDerivedDerived8(dv, dvExp)
    integer, parameter :: KEXP  = 8
    class(Derived(*,KEXP)), intent(in) :: dv, dvExp
    chkDerivedDerived8 = chkBase(dv%Base, dvExp%ch, dvExp%l)
    if (dv%l /= dvExp%l .or. dv%k /= KEXP) chkDerivedDerived8 = chkDerivedDerived8 + 8
    if (dv%lfld .neqv. dvExp%lfld .or. kind(dv%lfld) /= KEXP) chkDerivedDerived8 = chkDerivedDerived8 + 16
    if (size(dv%ifld) /= dvExp%l .or. kind(dv%ifld) /= KEXP .or. any(dv%ifld /= dvExp%ifld)) chkDerivedDerived8 = chkDerivedDerived8 + 32
    if (kind(dv%rfld) /= KEXP .or. .not.isSameReal8(dv%rfld,dvExp%rfld)) chkDerivedDerived8 = chkDerivedDerived8 + 64
  end function chkDerivedDerived8

  elemental integer function chkLocalDerived8(chVal, rVal, lVal, iVal, lenVal)
    integer, parameter :: KEXP = 8

    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal

    type(Derived(len(chVal),KEXP)) :: tmp

    tmp = Derived(len(chVal),KEXP)(chVal,rVal,lVal,iVal%iArr)
    chkLocalDerived8 = chkDerived8(tmp, chVal, rVal, lVal, iVal, lenVal)
  end function chkLocalDerived8

  elemental function map2Derived8(chVal, rVal, lVal, iVal, lenVal)
    integer, parameter :: KEXP = 8
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal
    type(Derived(2,KEXP)) :: map2Derived8
    map2Derived8 = Derived(2,KEXP)(chVal,rVal,lVal,iVal%iArr)
  end function map2Derived8


  elemental integer function chkD2_48(d2v, dExp, chExp, rExp, lExp, iExp, iExp2, lenExp)
    integer, parameter :: KEXP  = 4
    integer, parameter :: KEXP2 = 8
    class(D2(*,KEXP,KEXP2,*)), intent(in) :: d2v
    class(Derived(*,KEXP2)), intent(in) :: dExp
    character(*), intent(in) :: chExp
    real(KEXP), intent(in) :: rExp
    logical(KEXP), intent(in) :: lExp
    type(IArrWrapper(KEXP,*)), intent(in) :: iExp
    type(IArrWrapper(KEXP2,*)), intent(in) :: iExp2
    integer, intent(in) :: lenExp

    chkD2_48 = chkDerived4(d2v%Derived, chExp, rExp, lExp, iExp, lenExp) + 64 * chkDerivedDerived8(d2v%der, dExp)
    if (any(ubound(d2v%iArr) /= [lenExp,dExp%l]) .or. kind(d2v%iArr) /= KEXP2 .or. any([d2v%iArr] /= iExp2%iArr)) chkD2_48 = chkD2_48 + 512

  end function chkD2_48

  elemental integer function chkLocalD2_48(dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
    integer, parameter :: KEXP  = 4
    integer, parameter :: KEXP2 = 8
    class(Derived(*,KEXP2)), intent(in) :: dVal
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    type(IArrWrapper(KEXP2,*)), intent(in) :: iVal2
    integer, intent(in) :: lenVal
    type(D2(len(chVal),KEXP,KEXP2,len(dVal%ch))) :: tmp

    tmp = D2(len(chVal),KEXP,KEXP2,len(dVal%ch))(chVal, rVal, lVal, iVal%iArr, reshape(iVal2%iArr,[lenVal,dVal%l]), dVal)
    chkLocalD2_48 = chkD2_48(tmp, dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
  end function chkLocalD2_48

  elemental function map2D2_48(dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
    integer, parameter :: KEXP = 4
    integer, parameter :: KEXP2 = 8
    class(Derived(*,KEXP2)), intent(in) :: dVal
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal
    type(IArrWrapper(KEXP2,*)), intent(in) :: iVal2
    type(D2(1,KEXP,KEXP2,2)) :: map2D2_48

    map2D2_48 = D2(1,KEXP,KEXP2,2)(chVal,rVal,lVal, iVal%iArr, reshape(iVal2%iArr,[lenVal,dVal%l]), dVal)
  end function map2D2_48


  elemental integer function chkD2_84(d2v, dExp, chExp, rExp, lExp, iExp, iExp2, lenExp)
    integer, parameter :: KEXP  = 8
    integer, parameter :: KEXP2 = 4
    class(D2(*,KEXP,KEXP2,*)), intent(in) :: d2v
    class(Derived(*,KEXP2)), intent(in) :: dExp
    character(*), intent(in) :: chExp
    real(KEXP), intent(in) :: rExp
    logical(KEXP), intent(in) :: lExp
    type(IArrWrapper(KEXP,*)), intent(in) :: iExp
    type(IArrWrapper(KEXP2,*)), intent(in) :: iExp2
    integer, intent(in) :: lenExp

    chkD2_84 = chkDerived8(d2v%Derived, chExp, rExp, lExp, iExp, lenExp) + 64 * chkDerivedDerived4(d2v%der, dExp)
    if (any(ubound(d2v%iArr) /= [lenExp,dExp%l]) .or. kind(d2v%iArr) /= KEXP2 .or. any([d2v%iArr] /= iExp2%iArr)) chkD2_84 = chkD2_84 + 512

  end function chkD2_84

  elemental integer function chkLocalD2_84(dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
    integer, parameter :: KEXP  = 8
    integer, parameter :: KEXP2 = 4
    class(Derived(*,KEXP2)), intent(in) :: dVal
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    type(IArrWrapper(KEXP2,*)), intent(in) :: iVal2
    integer, intent(in) :: lenVal
    type(D2(len(chVal),KEXP,KEXP2,len(dVal%ch))) :: tmp

    tmp = D2(len(chVal),KEXP,KEXP2,len(dVal%ch))(chVal, rVal, lVal, iVal%iArr, reshape(iVal2%iArr,[lenVal,dVal%l]), dVal)
    chkLocalD2_84 = chkD2_84(tmp, dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
  end function chkLocalD2_84

  elemental function map2D2_84(dVal, chVal, rVal, lVal, iVal, iVal2, lenVal)
    integer, parameter :: KEXP = 8
    integer, parameter :: KEXP2 = 4
    class(Derived(*,KEXP2)), intent(in) :: dVal
    character(*), intent(in) :: chVal
    real(KEXP), intent(in) :: rVal
    logical(KEXP), intent(in) :: lVal
    type(IArrWrapper(KEXP,*)), intent(in) :: iVal
    integer, intent(in) :: lenVal
    type(IArrWrapper(KEXP2,*)), intent(in) :: iVal2
    type(D2(2,KEXP,KEXP2,2)) :: map2D2_84

    map2D2_84 = D2(2,KEXP,KEXP2,2)(chVal,rVal,lVal, iVal%iArr, reshape(iVal2%iArr,[lenVal,dVal%l]), dVal)
  end function map2D2_84


end module dtpIAssignElementalLocalFromVarsmod


program dtpIAssignElementalLocalFromVars

  use :: dtpIAssignElementalLocalFromVarsmod

  implicit none
  integer, parameter :: KEXP = 4
  integer, parameter :: KEXP2 = 8
  logical(KEXP), parameter :: T = .TRUE., F = .FALSE.
  integer(KEXP) :: i, j, r3(3), r2(2)
  type(Derived(2,KEXP)) :: d34(2)
  type(Derived(2,KEXP2)) :: d38(3)
  type(D2(1,KEXP,KEXP2,2)) :: d2_48(3)
  type(D2(2,KEXP2,KEXP,2)) :: d2_84(2)


  print *, map2KBase4([1,0,123456789])
  r3 = chkKBase4(map2KBase4([1,0,123456789]), [1,0,123456789])
  print *, "chkKBase(map2KBase4([1,0,123456789]), [1,0,123456789], KEXP): ", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))

  r3 = chkLocalKBase4([1,0,123456789])
  print *, "chkLocalKBase([1,0,123456789], KEXP): ", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))


  print *, map2Base(['abc',' 0 ','xyz'])
  r3 = chkBase(map2Base(['abc',' 0 ','xyz']), ['abc',' 0 ','xyz'], 3)
  print *, "chkBase(map2Base(['abc',' 0 ','xyz']), ['abc',' 0 ','xyz'], 3):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))

  r3 = chkLocalBase(['Tico Tico','Wheels   ','Guajira  '], 9)
  print *, "chkLocalBase(['Tico Tico','Wheels   ','Guajira  '], 9):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))


  d34 = map2Derived4(['ab','xy'], [0.1, 0.3], [T,F], [IArrWrapper(4,2)([-65535,1000000]), IArrWrapper(4,2)([123456789,98765432])], 2)
  print *, d34
  r2 = chkDerived4(d34, ['ab','xy'], [0.1, 0.3], [T,F], [IArrWrapper(4,2)([-65535,1000000]), IArrWrapper(4,2)([123456789,98765432])], 2)
  print *, "chkDerived4(map2Derived4(...)):", r2
  if (any(r2 /= 0)) call zzrc(maxval(r2))

  r2 = chkLocalDerived4(['Tico','ociT'], [0.1, 0.123], [F,T], [IArrWrapper(4,4)([46368, 75025, 121393, 196418]), IArrWrapper(4,4)([362880, 3628800, 39916800, 479001600])], 4)
  print *, "chkLocalDerived4(...):", r2
  if (any(r2 /= 0)) call zzrc(maxval(r2))


  d38 = map2Derived8(['bc','0 ','yz'], [10.1d-1, 10.2d-2, 0.123d-20], [logical(8):: F,T,T], &
                     [IArrWrapper(8,2)([479001600362880_8, 362880039916800_8]), IArrWrapper(8,2)([-362880039916800_8, -479001600362880_8]), IArrWrapper(8,2)([-1_8, 0_8])], 2)
  print *, d38
  r3 = chkDerived8(d38, ['bc','0 ','yz'], [10.1d-1, 10.2d-2, 0.123d-20], [logical(8):: F,T,T], &
                   [IArrWrapper(8,2)([479001600362880_8, 362880039916800_8]), IArrWrapper(8,2)([-362880039916800_8, -479001600362880_8]), IArrWrapper(8,2)([-1_8, 0_8])], 2)
  print *, "chkDerived8(map2Derived8(...)): ", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))

  r3 = chkLocalDerived8(['T','W','G'], [10.1d-1, 10.2d-2, 10.3d-10], [logical(8):: T,T,F], [IArrWrapper(8,1)([1]), IArrWrapper(8,1)([2]), IArrWrapper(8,1)([65535])], 1)
  print *, "chkLocalDerived8(...):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))



  d2_48 = map2D2_48(d38, ['T','W','G'], [0.1, 0.2, 0.3], [T,F,T], [IArrWrapper(4,1)([1]), IArrWrapper(4,1)([2]), IArrWrapper(4,1)([65535])], &
                    [IArrWrapper(8,3)([10000000025_8,10000000036_8,10000000081_8]), IArrWrapper(8,3)([1000000001001_8,1000000002001_8,1000000003001_8]), &
                    IArrWrapper(8,3)([10000000065535_8,100000000131000_8,100000000275000_8])], 1)
  print *, d2_48
  r3 = chkD2_48(d2_48, d38, ['T','W','G'], [0.1, 0.2, 0.3], [T,F,T], [IArrWrapper(4,1)([1]), IArrWrapper(4,1)([2]), IArrWrapper(4,1)([65535])], &
               [IArrWrapper(8,3)([10000000025_8,10000000036_8,10000000081_8]), IArrWrapper(8,3)([1000000001001_8,1000000002001_8,1000000003001_8]), &
                IArrWrapper(8,3)([10000000065535_8,100000000131000_8,100000000275000_8])], 1)
  print *, "chkD2_48(map2D2_48(...)):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))

  r3 = chkLocalD2_48(d38, ['T','W','G'], [0.1, 0.2, 0.3], [T,F,T], [IArrWrapper(4,1)([1]), IArrWrapper(4,1)([2]), IArrWrapper(4,1)([65535])], &
                          [IArrWrapper(8,3)([10000000025_8,10000000036_8,10000000081_8]), IArrWrapper(8,3)([1000000001001_8,1000000002001_8,1000000003001_8]), &
                           IArrWrapper(8,3)([10000000065535_8,100000000131000_8,100000000275000_8])], 1)
  print *, "chkLocalD2_48(...):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))


  d2_84 = map2D2_84(d34, ['ax','zb'], [10.1d-2, 10.3d-10], [logical(8):: F,T], &
                    [IArrWrapper(8,2)([10000000036_8,10000000081_8]), IArrWrapper(8,2)([1000000002001_8,1000000003001_8])], &
                    [IArrWrapper(4,4)([1_8,2_8,3_8,4_8]), IArrWrapper(4,4)([5_8,6_8,7_8,8_8])], 2)
  print *, d2_84
  r2 = chkD2_84(d2_84, d34, ['ax','zb'], [10.1d-2, 10.3d-10], [logical(8):: F,T], &
                [IArrWrapper(8,2)([10000000036_8,10000000081_8]), IArrWrapper(8,2)([1000000002001_8,1000000003001_8])], &
                [IArrWrapper(4,4)([1_8,2_8,3_8,4_8]), IArrWrapper(4,4)([5_8,6_8,7_8,8_8])], 2)
  print *, "chkD2_84(map2D2_84(...)): ", r2
  if (any(r2 /= 0)) call zzrc(maxval(r2))

  r3 = chkLocalD2_84([d34,d34(1)], ['TiTico','Wheels','Guajir'], [1.1d0, 1.1d-1, 1.1d-2], [logical(8):: F,T,T], &
                     [(IArrWrapper(8,6)([(i,i=1,6)]), j=1,3)], [(IArrWrapper(4,18)([(i,i=1,18)]), j=1,3)], 6)
  print *, "chkLocalD2_84(...):", r3
  if (any(r3 /= 0)) call zzrc(maxval(r3))


end program dtpIAssignElementalLocalFromVars
