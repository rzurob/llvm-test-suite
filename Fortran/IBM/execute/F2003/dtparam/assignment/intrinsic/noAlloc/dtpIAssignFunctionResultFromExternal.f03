!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign to function result in external procedure
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
!*  In external procedures, assign values to a function result of a parameterised
!*  derived type for which there is no user-defined assignment, and verify that
!*  the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignFunctionResultFromExternalmod

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

end module dtpIAssignFunctionResultFromExternalmod


program dtpIAssignFunctionResultFromExternal

  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none

  interface

     function baseFun1(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)) :: baseFun1
     end function baseFun1

     function baseFun2(n)
       import :: Base
       integer, intent(in) :: n
       type(Base(n)) :: baseFun2
     end function baseFun2

     function derivedFun4(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,4)) :: derivedFun4
     end function derivedFun4

     function derivedFun8(n)
       import :: Derived
       integer, intent(in) :: n
       type(Derived(n,8)) :: derivedFun8
     end function derivedFun8

     function d2Fun48(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,4,8,n2)) :: d2Fun48
     end function d2Fun48

     function d2Fun84(n,n2)
       import :: D2
       integer, intent(in) :: n, n2
       type(D2(n,8,4,n2)) :: d2Fun84
     end function d2Fun84

     subroutine testBase1(b)
       import :: Base
       type(Base(*)), intent(in) :: b
     end subroutine testBase1

     subroutine testBase2(b)
       import :: Base
       type(Base(*)), intent(in) :: b
     end subroutine testBase2

     subroutine testDerived4(d)
       import :: Derived
       type(Derived(*,4)), intent(in) :: d
     end subroutine testDerived4

     subroutine testDerived8(d)
       import :: Derived
       type(Derived(*,8)), intent(in) :: d
     end subroutine testDerived8

     subroutine testD2_48(d2v)
       import :: D2
       type(D2(*,4,8,*)), intent(in) :: d2v
     end subroutine testD2_48

     subroutine testD2_84(d2v)
       import :: D2
       type(D2(*,8,4,*)), intent(in) :: d2v
     end subroutine testD2_84

  end interface

  call testBase1(baseFun1(3))
  call testBase2(baseFun2(5))
  call testDerived4(derivedFun4(3))
  call testDerived8(derivedFun8(5))
  call testD2_48(d2Fun48(3,2))
  call testD2_84(d2Fun84(5,1))
  print *, 'done'

end program dtpIAssignFunctionResultFromExternal


function baseFun1(n)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n
  type(Base(n)) :: baseFun1
  baseFun1 = Base(n)('abc')
end function baseFun1


function baseFun2(n)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n
  type(Base(n)) :: baseFun2
  baseFun2 = Base(n)('abcde')
end function baseFun2


function derivedFun4(n)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,4)) :: derivedFun4
  derivedFun4 = Derived(n,4)('def',4.1_4,.true.,[1111111111,2122222222,1333333333])
end function derivedFun4


function derivedFun8(n)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n
  type(Derived(n,8)) :: derivedFun8
  derivedFun8 = Derived(n,8)('defgh',1.23456789D11,.true., &
                             [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8])
end function derivedFun8

function d2Fun48(n,n2)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,4,8,n2)) :: d2Fun48
  d2Fun48 = D2(n,4,8,n2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                         reshape([(1111_2*i,i=1,6)],[3,2]), &
                         derived(2,8)('xz',11235.81321D34,.true.,[76543211234567_8,-123456787654321_8]))
end function d2Fun48


function d2Fun84(n,n2)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  integer, intent(in) :: n, n2
  integer :: i
  type(D2(n,8,4,n2)) :: d2Fun84
  d2Fun84 = D2(n,8,4,n2)('defij',9.87654321D-12,.true., &
                         [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                         reshape([(1111_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))
end function d2Fun84


subroutine testBase1(b)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  type(Base(*)), intent(in) :: b
  print *, b
  if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'abc') error stop 2
end subroutine testBase1


subroutine testBase2(b)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  type(Base(*)), intent(in) :: b
  print *, b
  if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'abcde') error stop 12
end subroutine testBase2


subroutine testDerived4(d)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  type(Derived(*,4)), intent(in) :: d
  logical(4) :: precision_r4
  external :: precision_r4
  print *, d
  if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'def' .or. .not.d%lfld &
       .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
       .or. any(d%ifld /= [1111111111,2122222222,1333333333]) .or. .not.precision_r4(d%rfld,4.1_4)) error stop 3
end subroutine testDerived4


subroutine testDerived8(d)
  use :: dtpIAssignFunctionResultFromExternalmod
  implicit none
  type(Derived(*,8)), intent(in) :: d
  logical(4) :: precision_r8
  external :: precision_r8
  print *, d
  if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'defgh' .or. .not.d%lfld &
       .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
       .or. any(d%ifld /= [1111111111111_8,2222222222222_8,3333333333333_8,4444444444444_8,5555555555555_8]) &
       .or. .not.precision_r8(d%rfld,1.23456789D11)) error stop 13
end subroutine testDerived8


subroutine testD2_48(d2v)
  use :: dtpIAssignFunctionResultFromExternalmod
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
       .or. any([d2v%iarr] /= [1111,2222,3333,4444,5555,6666])) error stop 4
  if (d2v%der%l /= 2 .or. d2v%der%k /= 8 .or. len(d2v%der%ch) /= 2 &
       .or. d2v%der%ch /= 'xz' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 2 &
       .or. kind(d2v%der%lfld) /= 8 .or. kind(d2v%der%ifld) /= 8 .or. kind(d2v%der%rfld) /= 8 &
       .or. any(d2v%der%ifld /= [76543211234567_8,-123456787654321_8]) &
       .or. .not.precision_r8(d2v%der%rfld,11235.81321D34)) error stop 5
end subroutine testD2_48


subroutine testD2_84(d2v)
  use :: dtpIAssignFunctionResultFromExternalmod
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
       .or. any([d2v%iarr] /= [1111,2222,3333,4444,5555])) error stop 14
  if (d2v%der%l /= 1 .or. d2v%der%k /= 4 .or. len(d2v%der%ch) /= 1 &
       .or. d2v%der%ch /= 'y' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 1 &
       .or. kind(d2v%der%lfld) /= 4 .or. kind(d2v%der%ifld) /= 4 .or. kind(d2v%der%rfld) /= 4 &
       .or. any(d2v%der%ifld /= [-12345678_4]) &
       .or. .not.precision_r4(d2v%der%rfld,9.87654E-12)) error stop 15
end subroutine testD2_84