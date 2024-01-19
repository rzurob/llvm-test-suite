!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (container) via function in intrinsic assignment statement
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasic003 (<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  In a function, we assign values to the return variable and outside, assign the
!*  returned value to a variable.  Here, we have types with kind and length individually
!*  or together.  The function is defined in the module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasicFunction003mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
   contains
     final :: f1, f4
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
   contains
     final :: fin
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)) :: dkvar = dk(k)()
     type(dl(l)) :: dlvar
   contains
     final :: fin1, fin4
  end type container

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dk(1)) :: a
    print *, step, "f1:", a%ivar
    if( a%k /= 1 .or. a%k /= kind(a%ivar)) error stop 2
  end subroutine f1

  subroutine f4(a)
    type(dk(4)) :: a
    print *, step, "f4:", a%ivar
    if( a%k /= 4 .or. a%k /= kind(a%ivar)) error stop 3
  end subroutine f4

  subroutine fin(a)
    type(dl(*)) :: a
!    print *, step, "fin:", a%l, ">", a%chvar, "<", a%iarr
    if( a%l /= len(a%chvar) .or. a%l /= size(a%iarr)) error stop 4
  end subroutine fin

  subroutine fin1(a)
    type(container(1,*)) :: a
!    print *, step, "fin1:", a%l, a%dkvar, a%dlvar
    if (a%k /= 1 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) error stop 5
    if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l .or. size(a%dlvar%iarr) /= a%l) error stop 6
  end subroutine fin1

  subroutine fin4(a)
    type(container(4,*)) :: a
    print *, step, "fin4:", a%l, a%dkvar, a%dlvar
    if (a%k /= 4 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) error stop 7
    if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l .or. size(a%dlvar%iarr) /= a%l) error stop 8
  end subroutine fin4

  function funk1a(i)
    integer, parameter :: k = 1
    type(dk(k)), allocatable :: funk1a
    integer :: i
    print *, step, "in funk1a:", i
    funk1a = dk(k)(i)
  end function funk1a

  function funk4a(i)
    integer, parameter :: k = 4
    type(dk(k)), allocatable :: funk4a
    integer(k) :: i
    print *, step, "in funk4a:", i
    funk4a = dk(k)(i)
  end function funk4a

  function funk1(i)
    integer, parameter :: k = 1
    type(dk(k)) :: funk1
    integer :: i
    print *, step, "in funk1:", i
    funk1 = dk(k)(i)
  end function funk1

  function funk4(i)
    integer, parameter :: k = 4
    type(dk(k)) :: funk4
    integer(k) :: i
    print *, step, "in funk4:", i
    funk4 = dk(k)(i)
  end function funk4

  function funla(l,str,iarr)
    type(dl(:)), allocatable :: funla
    character(*) :: str
    integer :: l, iarr(*), i
    print *, step, "in funla:", l, str, (iarr(i), i=1,l)
    funla = dl(l)(str,iarr(:l))
  end function funla

  function funl(l,str,iarr)
    type(dl(l)) :: funl
    character(*) :: str
    integer :: l, iarr(*), i
    print *, step, "in funl:", l, str, (iarr(i), i=1,l)
    funl = dl(l)(str,iarr(:l))
  end function funl

  function func1a(l,i,str,iarr)
    integer, parameter :: k = 1
    type(container(k,:)), allocatable :: func1a
    integer :: l, i, iarr(*), j
    character(*) :: str
    print *, step, "in func1a:", l, i, str, (iarr(j), j=1,l)
    func1a = container(k,l)(funk1a(i), funla(l,str,iarr))
  end function func1a

  function func4a(l,i,str,iarr)
    integer, parameter :: k = 4
    type(container(k,:)), allocatable :: func4a
    integer :: l, iarr(*), j
    integer(k) :: i
    character(*) :: str
    print *, step, "in func4a:", l, i, str, (iarr(j), j=1,l)
    func4a = container(k,l)(funk4a(i), funla(l,str,iarr))
  end function func4a

  function func1(l,i,str,iarr)
    integer, parameter :: k = 1
    type(container(k,l)) :: func1
    integer :: l, i, iarr(*), j
    character(*) :: str
    print *, step, "in func1:", l, i, str, (iarr(j), j=1,l)
    func1 = container(k,l)(funk1(i), funl(l,str,iarr))
  end function func1

  function func4(l,i,str,iarr)
    integer, parameter :: k = 4
    type(container(k,l)) :: func4
    integer :: l, iarr(*), j
    integer(k) :: i
    character(*) :: str
    print *, step, "in func4:", l, i, str, (iarr(j), j=1,l)
    func4 = container(k,l)(funk4(i), funl(l,str,iarr))
  end function func4

end module dtpIAABasicFunction003mod



program dtpIAABasicFunction003

  use dtpIAABasicFunction003mod
  implicit none

  type(container(1,:)), allocatable :: o1, o2, o3
  type(container(4,:)), allocatable :: v1, v2, v3

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":
  step = 10
  print *, step, allocated(o1), allocated(o2), allocated(o3)

  step = 11
  print *, step, "o1 = func1(2,34,'ab',[35,36])"
  step = 12
  o1 = func1(2,34,'ab',[35,36])

  step = 13
  print *, step, "o2 = func1(2,37,'cd',[38,39])"
  step = 14
  o2 = func1(2,37,'cd',[38,39])

  step = 15
  print *, step, "o1 = o2 {o1=", allocated(o1), o1, ", o2=", allocated(o2), o2, "}"
  step = 16
  o1 = o2

  step = 17
  print *, step, "o3 = func1(3,40,'efg',[41,42,43])"
  step = 18
  o3 = func1(3,40,'efg',[41,42,43])

  step = 19
  print *, step, "o2 = o3 {o2=", allocated(o2), o2, ", o3=", allocated(o3), o3, "}"
  step = 20
  o2 = o3

  ! Repeat the above with a container of kind 4
  step = 21
  print *, step, allocated(v1), allocated(v2), allocated(v3)

  step = 22
  print *, step, "v1 = func4(2,44,'AB',[45,46])"
  step = 23
  v1 = func4(2,44,'AB',[45,46])

  step = 24
  print *, step, "v2 = func4(2,47,'CD',[48,49])"
  step = 25
  v2 = func4(2,47,'CD',[48,49])

  step = 26
  print *, step, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  step = 27
  v1 = v2

  step = 28
  print *, step, "v3 = func4(3,50,'EFG',[51,52,53])"
  step = 29
  v3 = func4(3,50,'EFG',[51,52,53])

  step = 30
  print *, step, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  step = 31
  v2 = v3

  ! Just use a higher number in place of "step":
  print *, 41, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), size(o1%dlvar%iarr), o1
  print *, 42, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), size(o2%dlvar%iarr), o2
  print *, 43, allocated(o3), o3%k, o3%l, len(o3%dlvar%chvar), size(o3%dlvar%iarr), o3

  print *, 44, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), size(v1%dlvar%iarr), v1
  print *, 45, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), size(v2%dlvar%iarr), v2
  print *, 46, allocated(v3), v3%k, v3%l, len(v3%dlvar%chvar), size(v3%dlvar%iarr), v3

  deallocate(o1, o2, o3, v1, v2, v3)

  step = 50
  print *, step, allocated(o1), allocated(o2), allocated(o3)

  step = 51
  print *, step, "o1 = func1a(2,34,'ab',[35,36])"
  step = 52
  o1 = func1a(2,34,'ab',[35,36])

  step = 53
  print *, step, "o2 = func1a(2,37,'cd',[38,39])"
  step = 54
  o2 = func1a(2,37,'cd',[38,39])

  step = 55
  print *, step, "o1 = o2 {o1=", allocated(o1), o1, ", o2=", allocated(o2), o2, "}"
  step = 56
  o1 = o2

  step = 57
  print *, step, "o3 = func1a(3,40,'efg',[41,42,43])"
  step = 58
  o3 = func1a(3,40,'efg',[41,42,43])

  step = 59
  print *, step, "o2 = o3 {o2=", allocated(o2), o2, ", o3=", allocated(o3), o3, "}"
  step = 60
  o2 = o3

  ! Repeat the above with a container of kind 4
  step = 61
  print *, step, allocated(v1), allocated(v2), allocated(v3)

  step = 62
  print *, step, "v1 = func4a(2,44,'AB',[45,46])"
  step = 63
  v1 = func4a(2,44,'AB',[45,46])

  step = 64
  print *, step, "v2 = func4a(2,47,'CD',[48,49])"
  step = 65
  v2 = func4a(2,47,'CD',[48,49])

  step = 66
  print *, step, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  step = 67
  v1 = v2

  step = 68
  print *, step, "v3 = func4a(3,50,'EFG',[51,52,53])"
  step = 69
  v3 = func4a(3,50,'EFG',[51,52,53])

  step = 70
  print *, step, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  step = 71
  v2 = v3

  ! Just use a higher number in place of "step":
  print *, 91, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), size(o1%dlvar%iarr), o1
  print *, 92, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), size(o2%dlvar%iarr), o2
  print *, 93, allocated(o3), o3%k, o3%l, len(o3%dlvar%chvar), size(o3%dlvar%iarr), o3

  print *, 94, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), size(v1%dlvar%iarr), v1
  print *, 95, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), size(v2%dlvar%iarr), v2
  print *, 96, allocated(v3), v3%k, v3%l, len(v3%dlvar%chvar), size(v3%dlvar%iarr), v3

end program dtpIAABasicFunction003
