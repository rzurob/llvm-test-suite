!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCDummyNPassIntrInterfaceKFunIntr
!*
!*  DATE                       : 2009-03-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to function with intrinsic dummy args but no passed-object dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions which do not
!*  expect a passed-object dummy argument (i.e., they are NOPASS), but which
!*  do expect an argument of intrinsic type.  Use local variables to access the
!*  pointers, invoked in the context of an argument to a subroutine call.
!*  Define a parameterised derived type with two procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times as an argument to a subroutine call, testing the type of the returned
!*  value.
!*
!*  Summary: pointer to no-pass function declared in kind type, referenced by dummy
!*  argument, specified via interface, expecting intrinsic arg(s), returning intrinsic
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassIntrInterfaceKFunIntrmod

  implicit none
  type dt (k)
     integer, kind :: k
     character(5) :: chval
     integer(k) :: ival
     procedure (fChar), pointer, nopass :: p1 => null()
     procedure (fInt2), pointer, nopass :: p2 => null()
  end type dt

  abstract interface

     character(5) function fChar(a1)
       integer(2), intent(in) :: a1
     end function fChar

     integer(2) function fInt2(a1)
       character(5), intent(in) :: a1
     end function fInt2

  end interface

  character(5), save :: english(-1:11) = [character(5):: "neg", "zero", "one", "two", "three", "four", "five", &
                                                         "six", "seven", "eight", "nine", "ten", "big"]

  character(5), save :: roman(-1:11) = [character(5):: "what?", "dunno", "i", "ii", "iii", "iv", "v", &
                                                       "vi", "vii", "viii", "ix", "x", "mmix"]
contains


  character(5) function getCharA(ival)
    integer(2), intent(in) :: ival
    getCharA = english(max(lbound(english,1,2),min(ubound(english,1,2),ival)))
  end function getCharA

  character(5) function getCharB(ival)
    integer(2), intent(in) :: ival
    getCharB = roman(max(lbound(roman,1,2),min(ubound(roman,1,2),ival)))
  end function getCharB

  integer(2) function getInt2A(str)
    character(5) :: str
    integer(2) :: i
    do i = lbound(english,1),ubound(english,1)
       if (str == english(i)) exit
    end do
    getInt2A = i
  end function getInt2A

  integer(2) function getInt2B(str)
    character(5) :: str
    integer(2) :: i
    do i = lbound(roman,1),ubound(roman,1)
       if (str == roman(i)) exit
    end do
    getInt2B = i
  end function getInt2B

end module dtpPPCDummyNPassIntrInterfaceKFunIntrmod


program dtpPPCDummyNPassIntrInterfaceKFunIntr

  use dtpPPCDummyNPassIntrInterfaceKFunIntrmod
  implicit none

  type(dt(1)) :: e1a, e1b, r1a, r1b ! english and roman
  type(dt(2)) :: e2a, e2b, r2a, r2b
  integer(2) :: i

  ! test our test code:
  print *, (" ", getCharA(i), i=-2,12)
  print *, (" ", getCharB(i), i=-2,12)
  print *, getInt2A("english"), (getInt2A(english(i)), i=lbound(english,1),ubound(english,1))
  print *, getInt2B("roman"), (getInt2B(roman(i)), i=lbound(roman,1),ubound(roman,1))
  print *

  e1a = dt(1)('five ', 12, getCharA, getInt2A)
  e1b = dt(1)('eight',  9, getCharB, getInt2B)
  e2a = dt(2)('zort ',  5, getCharA, getInt2A)
  e2b = dt(2)('zero ', -1, getCharB, getInt2B)

  r1a = dt(1)('v    ', 12, getCharA, getInt2A)
  r1b = dt(1)('viii ',  9, getCharB, getInt2B)
  r2a = dt(2)('troz ',  5, getCharA, getInt2A)
  r2b = dt(2)('dunno', -1, getCharB, getInt2B)

  call test("e1a % p1(int(e1a%ival,2))", e1a % p1(int(e1a%ival,2)))
  call test("e1a % p1(int(e1b%ival,2))", e1a % p1(int(e1b%ival,2)))
  call test("e1a % p1(e2a%ival)", e1a % p1(e2a%ival))
  call test("e1a % p1(e2b%ival)", e1a % p1(e2b%ival))
  call test("e1a % p2(e1a%chval)", e1a % p2(e1a%chval))
  call test("e1a % p2(e1b%chval)", e1a % p2(e1b%chval))
  call test("e1a % p2(e2a%chval)", e1a % p2(e2a%chval))
  call test("e1a % p2(e2b%chval)", e1a % p2(e2b%chval))

  print *

  call test("e1b % p1(int(e1a%ival,2))", e1b % p1(int(e1a%ival,2)))
  call test("e1b % p1(int(e1b%ival,2))", e1b % p1(int(e1b%ival,2)))
  call test("e1b % p1(e2a%ival)", e1b % p1(e2a%ival))
  call test("e1b % p1(e2b%ival)", e1b % p1(e2b%ival))
  call test("e1b % p2(r1a%chval)", e1b % p2(r1a%chval))
  call test("e1b % p2(r1b%chval)", e1b % p2(r1b%chval))
  call test("e1b % p2(r2a%chval)", e1b % p2(r2a%chval))
  call test("e1b % p2(r2b%chval)", e1b % p2(r2b%chval))
  print *

  call test("e2a % p1(int(e1a%ival,2))", e2a % p1(int(e1a%ival,2)))
  call test("e2a % p1(int(e1b%ival,2))", e2a % p1(int(e1b%ival,2)))
  call test("e2a % p1(e2a%ival)", e2a % p1(e2a%ival))
  call test("e2a % p1(e2b%ival)", e2a % p1(e2b%ival))
  call test("e2a % p2(e1a%chval)", e2a % p2(e1a%chval))
  call test("e2a % p2(e1b%chval)", e2a % p2(e1b%chval))
  call test("e2a % p2(e2a%chval)", e2a % p2(e2a%chval))
  call test("e2a % p2(e2b%chval)", e2a % p2(e2b%chval))
  print *

  call test("e2b % p1(int(e1a%ival,2))", e2b % p1(int(e1a%ival,2)))
  call test("e2b % p1(int(e1b%ival,2))", e2b % p1(int(e1b%ival,2)))
  call test("e2b % p1(e2a%ival)", e2b % p1(e2a%ival))
  call test("e2b % p1(e2b%ival)", e2b % p1(e2b%ival))
  call test("e2b % p2(r1a%chval)", e2b % p2(r1a%chval))
  call test("e2b % p2(r1b%chval)", e2b % p2(r1b%chval))
  call test("e2b % p2(r2a%chval)", e2b % p2(r2a%chval))
  call test("e2b % p2(r2b%chval)", e2b % p2(r2b%chval))
  print *

  e1a % p1 => getCharB
  e1a % p2 => getInt2B
  r1a % p1 => getCharB
  r1a % p2 => getInt2B

  e1b % p1 => getCharA
  e1b % p2 => getInt2A
  r1b % p1 => getCharA
  r1b % p2 => getInt2A

  e2a % p1 => getCharB
  e2a % p2 => getInt2B
  r2a % p1 => getCharB
  r2a % p2 => getInt2B

  e2b % p1 => getCharA
  e2b % p2 => getInt2A
  r2b % p1 => getCharA
  r2b % p2 => getInt2A

  call test("e1a % p1(int(e1a%ival,2))", e1a % p1(int(e1a%ival,2)))
  call test("e1a % p1(int(e1b%ival,2))", e1a % p1(int(e1b%ival,2)))
  call test("e1a % p1(e2a%ival)", e1a % p1(e2a%ival))
  call test("e1a % p1(e2b%ival)", e1a % p1(e2b%ival))
  call test("e1a % p2(r1a%chval)", e1a % p2(r1a%chval))
  call test("e1a % p2(r1b%chval)", e1a % p2(r1b%chval))
  call test("e1a % p2(r2a%chval)", e1a % p2(r2a%chval))
  call test("e1a % p2(r2b%chval)", e1a % p2(r2b%chval))
  print *

  call test("e1b % p1(int(e1a%ival,2))", e1b % p1(int(e1a%ival,2)))
  call test("e1b % p1(int(e1b%ival,2))", e1b % p1(int(e1b%ival,2)))
  call test("e1b % p1(e2a%ival)", e1b % p1(e2a%ival))
  call test("e1b % p1(e2b%ival)", e1b % p1(e2b%ival))
  call test("e1b % p2(e1a%chval)", e1b % p2(e1a%chval))
  call test("e1b % p2(e1b%chval)", e1b % p2(e1b%chval))
  call test("e1b % p2(e2a%chval)", e1b % p2(e2a%chval))
  call test("e1b % p2(e2b%chval)", e1b % p2(e2b%chval))
  print *

  call test("e2a % p1(int(e1a%ival,2))", e2a % p1(int(e1a%ival,2)))
  call test("e2a % p1(int(e1b%ival,2))", e2a % p1(int(e1b%ival,2)))
  call test("e2a % p1(e2a%ival)", e2a % p1(e2a%ival))
  call test("e2a % p1(e2b%ival)", e2a % p1(e2b%ival))
  call test("e2a % p2(r1a%chval)", e2a % p2(r1a%chval))
  call test("e2a % p2(r1b%chval)", e2a % p2(r1b%chval))
  call test("e2a % p2(r2a%chval)", e2a % p2(r2a%chval))
  call test("e2a % p2(r2b%chval)", e2a % p2(r2b%chval))
  print *

  call test("e2b % p1(int(e1a%ival,2))", e2b % p1(int(e1a%ival,2)))
  call test("e2b % p1(int(e1b%ival,2))", e2b % p1(int(e1b%ival,2)))
  call test("e2b % p1(e2a%ival)", e2b % p1(e2a%ival))
  call test("e2b % p1(e2b%ival)", e2b % p1(e2b%ival))
  call test("e2b % p2(e1a%chval)", e2b % p2(e1a%chval))
  call test("e2b % p2(e1b%chval)", e2b % p2(e1b%chval))
  call test("e2b % p2(e2a%chval)", e2b % p2(e2a%chval))
  call test("e2b % p2(e2b%chval)", e2b % p2(e2b%chval))
  print *

  print *, "done"


contains


  subroutine test(description, object)
    character(*), intent(in) :: description
    class(*), intent(in) :: object
    select type (object)
    type is (character(*)); print *, description, ": character, l=", len(object), ">", object, "<"
    type is (integer(2));   print *, description, ": i*2=", object
    class default;          print *, description, ": unknown type"
    end select
  end subroutine test

end program dtpPPCDummyNPassIntrInterfaceKFunIntr
