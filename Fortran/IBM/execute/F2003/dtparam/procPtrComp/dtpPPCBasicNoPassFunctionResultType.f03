!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to function without passed-object dummy argument - test type of result
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
!*  expect a passed-object dummy argument.  Use local variables to access the
!*  pointers in assignment statements and print statements.
!*  Define a parameterised derived type with two procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times in print statements, and then assign new procedure references and repeat.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCBasicNoPassFunctionResultTypemod

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
       import :: dt
       class(dt(1)), intent(in) :: a1
     end function fChar

     integer(2) function fInt2(a1)
       import :: dt
       class(dt(2)), intent(in) :: a1
     end function fInt2

  end interface


contains


  character(5) function getCharA(this)
    class(dt(1)), intent(in) :: this
    getCharA = this % chval
  end function getCharA

  character(5) function getCharB(this)
    class(dt(1)), intent(in) :: this
    getCharB = reverse(this % chval)
  end function getCharB

  character(5) function reverse(a1)
    character(5), intent(in) :: a1
    character(5) :: tmp5
    character(1) :: tmp1(5)
    equivalence (tmp1,tmp5)
    tmp5 = a1
    tmp1 = tmp1(5:1:-1)
    reverse = tmp5
  end function reverse


  integer(2) function getInt2A(this)
    class(dt(2)), intent(in) :: this
    getInt2A = this % ival
  end function getInt2A

  integer(2) function getInt2B(this)
    class(dt(2)), intent(in) :: this
    getInt2B = - this % ival
  end function getInt2B


end module dtpPPCBasicNoPassFunctionResultTypemod


program dtpPPCBasicNoPassFunctionResultType

  use dtpPPCBasicNoPassFunctionResultTypemod
  implicit none
  type(dt(1)) :: t1a, t1b
  type(dt(2)) :: t2a, t2b

  t1a = dt(1)('horse',127,getCharA,getInt2A)
  t1b = dt(1)('equus',-123,getCharB,getInt2B)
  t2a = dt(2)('abcde',32000,getCharA,getInt2A)
  t2b = dt(2)('fghij',-12345,getCharB,getInt2B)

  call test("t1a % p1(t1a)", t1a % p1(t1a))
  call test("t1a % p1(t1b)", t1a % p1(t1b))
  call test("t1a % p2(t2a)", t1a % p2(t2a))
  call test("t1a % p2(t2b)", t1a % p2(t2b))
  print *

  call test("t1b % p1(t1a)", t1b % p1(t1a))
  call test("t1b % p1(t1b)", t1b % p1(t1b))
  call test("t1b % p2(t2a)", t1b % p2(t2a))
  call test("t1b % p2(t2b)", t1b % p2(t2b))
  print *

  call test("t2a % p1(t1a)", t2a % p1(t1a))
  call test("t2a % p1(t1b)", t2a % p1(t1b))
  call test("t2a % p2(t2a)", t2a % p2(t2a))
  call test("t2a % p2(t2b)", t2a % p2(t2b))
  print *

  call test("t2b % p1(t1a)", t2b % p1(t1a))
  call test("t2b % p1(t1b)", t2b % p1(t1b))
  call test("t2b % p2(t2a)", t2b % p2(t2a))
  call test("t2b % p2(t2b)", t2b % p2(t2b))
  print *

  t1a % p1 => getCharB
  t1a % p2 => getInt2B

  t1b % p1 => getCharA
  t1b % p2 => getInt2A

  t2a % p1 => getCharB
  t2a % p2 => getInt2B

  t2b % p1 => getCharA
  t2b % p2 => getInt2A

  call test("t1a % p1(t1a)", t1a % p1(t1a))
  call test("t1a % p1(t1b)", t1a % p1(t1b))
  call test("t1a % p2(t2a)", t1a % p2(t2a))
  call test("t1a % p2(t2b)", t1a % p2(t2b))
  print *

  call test("t1b % p1(t1a)", t1b % p1(t1a))
  call test("t1b % p1(t1b)", t1b % p1(t1b))
  call test("t1b % p2(t2a)", t1b % p2(t2a))
  call test("t1b % p2(t2b)", t1b % p2(t2b))
  print *

  call test("t2a % p1(t1a)", t2a % p1(t1a))
  call test("t2a % p1(t1b)", t2a % p1(t1b))
  call test("t2a % p2(t2a)", t2a % p2(t2a))
  call test("t2a % p2(t2b)", t2a % p2(t2b))
  print *

  call test("t2b % p1(t1a)", t2b % p1(t1a))
  call test("t2b % p1(t1b)", t2b % p1(t1b))
  call test("t2b % p2(t2a)", t2b % p2(t2a))
  call test("t2b % p2(t2b)", t2b % p2(t2b))
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


end program dtpPPCBasicNoPassFunctionResultType