!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCBasicPassLenFunctionNamedArg
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : pointer to PASS functions from DTP with len param (named arg)
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCBasicPassLenFunction ()
!*
!*  DESCRIPTION
!*
!*  Create objects with different LEN params and invoke PASS functions on them.
!*  Functions are specified using an interface.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCBasicPassLenFunctionNamedArgmod

  implicit none

  type dt (l)
     integer, len :: l = 0
     character(l) :: chval = ''
     procedure (f), pointer, pass(this) :: p1 => null()
  end type dt

  type def
     type(dt(:)), pointer :: p => null()
  end type def

  abstract interface
     integer function f(that, this)
       import :: dt
       integer, intent(in) :: that
       class(dt(*)), intent(in) :: this
     end function f
  end interface

contains

  integer function fun0(int,dtArg)
    integer, intent(in) :: int
    class(dt(*)), intent(in) :: dtArg
    fun0 = dtArg % l + int
  end function fun0

  integer function fun1(int,dtArg)
    integer, intent(in) :: int
    class(dt(*)), intent(in) :: dtArg
    fun1 = len(dtArg % chval) * int
  end function fun1

  integer function fun2(int,dtArg)
    integer, intent(in) :: int
    class(dt(*)), intent(in) :: dtArg
    fun2 = len_trim(dtArg % chval) - int
  end function fun2

end module dtpPPCBasicPassLenFunctionNamedArgmod


program dtpPPCBasicPassLenFunctionNamedArg

  use dtpPPCBasicPassLenFunctionNamedArgmod
  implicit none
  type(dt(1)), target      :: t1
  type(dt(:)), target, allocatable :: ta2, ta3
  type(dt(:)), pointer     :: tp
  type(dt(4)), target      :: t4
  type(dt(5)), target      :: t5
  type(def) :: ref(6)
  integer :: i

  allocate(ta2, source=dt(2)('xx',fun0))
  allocate(dt(3):: ta3)

  ta3 % chval = 'three'
  t1  % chval = 'one'
  t4  % chval = 'four'
  t5  % chval = 'fuenf'
  tp => t4

  ref = [def(t1), def(ta2), def(ta3), def(tp), def(t4), def(t5)]

  ta3 % p1 => fun0
  t1  % p1 => fun0
  t4  % p1 => fun0
  t5  % p1 => fun0

  call test("fun0", 2, [1,2,3,4,4,5] + 2)

  ta3 % p1 => fun1
  ta2 % p1 => fun1
  t1  % p1 => fun1
  t4  % p1 => fun1
  t5  % p1 => fun1

  call test("fun1", 3, [1,2,3,4,4,5] * 3)

  ta3 % p1 => fun2
  ta2 % p1 => fun2
  t1  % p1 => fun2
  t4  % p1 => fun2
  t5  % p1 => fun2

  call test("fun2", -1, [1,2,3,4,4,5] - (-1))

contains

  subroutine test(title, a2, cmpArray)
    character(*) :: title
    integer, intent(in) :: cmpArray(:), a2
    integer :: i
    print *, title
    print *, t1 % p1(a2), ta2 % p1(a2), ta3 % p1(a2), tp % p1(a2), t4 % p1(a2), t5 % p1(a2)
    do i = 1, size(ref)
       print *, ref(i) % p % p1(a2)
    end do
    print *, [(ref(i) % p % p1(a2), i=1,size(ref))]
    if (any([(ref(i) % p % p1(a2), i=1,size(ref))] /= cmpArray)) stop 2
    if (any([t1 % p1(a2), ta2 % p1(a2), ta3 % p1(a2), tp % p1(a2), t4 % p1(a2), t5 % p1(a2)] /= cmpArray)) stop 3
  end subroutine test

end program dtpPPCBasicPassLenFunctionNamedArg
