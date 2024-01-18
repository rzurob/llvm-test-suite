!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign variables to function result in external procedure with auto length parameter pointers
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
!*  In external procedures, allocate structure constructors to assign values to
!*  variables from different contexts (module, local) of a parameterised derived
!*  type for which there is no user-defined assignment, and then assign those
!*  values to pointer function results.
!*  Verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignExternalFunctionResultFromVarsAutomod

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

end module dtpPtrAssignExternalFunctionResultFromVarsAutomod


program dtpPtrAssignExternalFunctionResultFromVarsAuto

  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none

  interface

     function baseFun1(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)), pointer :: baseFun1
     end function baseFun1

     function baseFun2(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)), pointer :: baseFun2
     end function baseFun2

     function derivedFun4(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,4)), pointer :: derivedFun4
     end function derivedFun4

     function derivedFun8(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,8)), pointer :: derivedFun8
     end function derivedFun8

     function d2Fun48(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,4,8,n2)), pointer :: d2Fun48
     end function d2Fun48

     function d2Fun84(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,8,4,n2)), pointer :: d2Fun84
     end function d2Fun84

     function baseFun1HA(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)), pointer :: baseFun1HA
     end function baseFun1HA

     function baseFun2HA(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)), pointer :: baseFun2HA
     end function baseFun2HA

     function derivedFun4HA(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,4)), pointer :: derivedFun4HA
     end function derivedFun4HA

     function derivedFun8HA(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,8)), pointer :: derivedFun8HA
     end function derivedFun8HA

     function d2Fun48HA(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,4,8,n2)), pointer :: d2Fun48HA
     end function d2Fun48HA

     function d2Fun84HA(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,8,4,n2)), pointer :: d2Fun84HA
     end function d2Fun84HA

     subroutine testBase1(b)
       import :: Base
       type(Base(*)), pointer, intent(in) :: b
     end subroutine testBase1

     subroutine testBase2(b)
       import :: Base
       type(Base(*)), pointer, intent(in) :: b
     end subroutine testBase2

     subroutine testDerived4(d)
       import :: Derived
       type(Derived(*,4)), pointer, intent(in) :: d
     end subroutine testDerived4

     subroutine testDerived8(d)
       import :: Derived
       type(Derived(*,8)), pointer, intent(in) :: d
     end subroutine testDerived8

     subroutine testD2_48(d2v)
       import :: D2
       type(D2(*,4,8,*)), pointer, intent(in) :: d2v
     end subroutine testD2_48

     subroutine testD2_84(d2v)
       import :: D2
       type(D2(*,8,4,*)), pointer, intent(in) :: d2v
     end subroutine testD2_84

  end interface

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

end program dtpPtrAssignExternalFunctionResultFromVarsAuto


function baseFun1(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Base(n)), pointer :: baseFun1
  type(Base(n)), pointer :: tmp
  allocate(tmp, source = Base(n)('abc'))
  baseFun1 => tmp
end function baseFun1


function baseFun1HA(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Base(n)), pointer :: baseFun1HA
  type(Base(n)), pointer :: ha
  call internal
  baseFun1HA => ha
contains
  subroutine internal
    allocate(ha, source = Base(n)('abc'))
  end subroutine internal
end function baseFun1HA


function baseFun2(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Base(n)), pointer :: baseFun2
  type(Base(n)), pointer :: tmp
  allocate(tmp, source = Base(n)('abcde'))
  baseFun2 => tmp
end function baseFun2


function baseFun2HA(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Base(n)), pointer :: baseFun2HA
  type(Base(n)), pointer :: tmp
  call internal
  baseFun2HA => tmp
contains
  subroutine internal
    allocate(tmp, source = Base(n)('abcde'))
  end subroutine internal
end function baseFun2HA


function derivedFun4(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,4)), pointer :: derivedFun4
  type(Derived(n,4)), pointer :: tmp
  allocate(tmp, source = Derived(n,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333]))
  derivedFun4 => tmp
end function derivedFun4


function derivedFun4HA(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,4)), pointer :: derivedFun4HA
  type(Derived(n,4)), pointer :: tmp
  call internal
  derivedFun4HA => tmp
contains
  subroutine internal
    allocate(tmp, source = Derived(n,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333]))
  end subroutine internal
end function derivedFun4HA


function derivedFun8(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,8)), pointer :: derivedFun8
  type(Derived(n,8)), pointer :: tmp
  allocate(tmp, source = Derived(n,8)('defgh',1.23456789D11,.true., [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]))
  derivedFun8 => tmp
end function derivedFun8


function derivedFun8HA(n)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,8)), pointer :: derivedFun8HA
  type(Derived(n,8)), pointer :: tmp
  call internal
  derivedFun8HA => tmp
contains
  subroutine internal
    allocate(tmp, source = Derived(n,8)('defgh',1.23456789D11,.true., [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]))
  end subroutine internal
end function derivedFun8HA

function d2Fun48(n,n2)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,4,8,n2)), pointer :: d2Fun48
  type(D2(n,4,8,n2)), pointer :: tmp
  allocate(tmp, source = D2(n,4,8,n2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                                      reshape([(1111_2*i,i=1,6)],[3,2]), derived(n2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8])))
  d2Fun48 => tmp
end function d2Fun48


function d2Fun48HA(n,n2)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,4,8,n2)), pointer :: d2Fun48HA
  type(D2(n,4,8,n2)), pointer :: tmp
  call internal
  d2Fun48HA => tmp
contains
  subroutine internal
    allocate(tmp, source = D2(n,4,8,n2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                                        reshape([(1111_2*i,i=1,6)],[3,2]), derived(n2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8])))
  end subroutine internal
end function d2Fun48HA


function d2Fun84(n,n2)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,8,4,n2)), pointer :: d2Fun84
  type(D2(n,8,4,n2)), pointer :: tmp
  allocate(tmp, source = D2(n,8,4,n2)('defij',9.87654321D-12,.true., [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                      reshape([(1111_2*i,i=1,5)],[5,1]),derived(n2,4)('y',9.87654E-12,.true.,[-12345678_4])))
  d2Fun84 => tmp
end function d2Fun84


function d2Fun84HA(n,n2)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,8,4,n2)), pointer :: d2Fun84HA
  type(D2(n,8,4,n2)), pointer :: tmp
  call internal
  d2Fun84HA => tmp
contains
  subroutine internal
    allocate(tmp, source = D2(n,8,4,n2)('defij',9.87654321D-12,.true., [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                        reshape([(1111_2*i,i=1,5)],[5,1]),derived(n2,4)('y',9.87654E-12,.true.,[-12345678_4])))
  end subroutine internal
end function d2Fun84HA


subroutine testBase1(b)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(Base(*)), intent(in), pointer :: b
  type(Base(:)), pointer :: tmp
  print *, b
  if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'abc') stop 2
  tmp => b
  deallocate(tmp) ! be neat
end subroutine testBase1


subroutine testBase2(b)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(Base(*)), intent(in), pointer :: b
  type(Base(:)), pointer :: tmp
  print *, b
  if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'abcde') stop 12
  tmp => b
  deallocate(tmp) ! be neat
end subroutine testBase2


subroutine testDerived4(d)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(Derived(*,4)), intent(in), pointer :: d
  type(Derived(:,4)), pointer :: tmp
  logical(4) :: precision_r4
  external :: precision_r4
  print *, d
  if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'def' .or. .not.d%lfld &
       .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
       .or. any(d%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d%rfld,4.1_4)) stop 3
  tmp => d
  deallocate(tmp) ! be neat
end subroutine testDerived4


subroutine testDerived8(d)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(Derived(*,8)), intent(in), pointer :: d
  type(Derived(:,8)), pointer :: tmp
  logical(4) :: precision_r8
  external :: precision_r8
  print *, d
  if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'defgh' .or. .not.d%lfld &
       .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
       .or. any(d%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
       .or. .not.precision_r8(d%rfld,1.23456789D11)) stop 13
  tmp => d
  deallocate(tmp) ! be neat
end subroutine testDerived8


subroutine testD2_48(d2v)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(D2(*,4,8,*)), intent(in), pointer :: d2v
  type(D2(:,4,8,:)), pointer :: tmp
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
  tmp => d2v
  deallocate(tmp) ! be neat
end subroutine testD2_48


subroutine testD2_84(d2v)
  use :: dtpPtrAssignExternalFunctionResultFromVarsAutomod
  implicit none
  type(D2(*,8,4,*)), intent(in), pointer :: d2v
  type(D2(:,8,4,:)), pointer :: tmp
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
  tmp => d2v
  deallocate(tmp) ! be neat
end subroutine testD2_84
