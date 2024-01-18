!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpSimpleBinaryTKR
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct binary operators are used for a given TKR
!*
!*  REFERENCE                  : Feature Number 361989
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
!*  Define several types with type-bound procedures and generic bindings for
!*  binary operators and verify that the correct function is invoked for a given TKR.
!*  The expressions in print statements and assignment statements can be tested for
!*  output, data value, and static type; the expressions in subroutine calls allow
!*  testing for type and length at run time.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleBinaryTKRmod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
   contains
     generic :: operator(+) => binaryPlusK2, binaryPlusK4, binaryPlusK4R1
     procedure, pass :: binaryPlusK2
     procedure, pass :: binaryPlusK4
     procedure, pass :: binaryPlusK4R1
  end type dk

contains

  elemental type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in) :: this, that
    binaryPlusK2 = dk(2)(this%ivar + that%ivar + 2)
  end function binaryPlusK2

  elemental type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK4 = dk(4)(this%ivar + that%ivar + 4)
  end function binaryPlusK4

  function binaryPlusK4R1(this,that)
    class(dk(4)), intent(in) :: this, that(:)
    type(dk(4)) :: binaryPlusK4R1(size(that))
    integer :: i
    binaryPlusK4R1 = [(dk(4)(this%ivar + that(i)%ivar + 8), i=1,size(that))]
  end function binaryPlusK4R1

end module dtpUOpSimpleBinaryTKRmod

program dtpUOpSimpleBinaryTKR
  use :: dtpUOpSimpleBinaryTKRmod
  implicit none

  type(dk(2)) :: xk2a, xk2b, xk2c, xk2d, ak2a(2), ak2b(2), ak2c(3), ak2d(3), xk2e, ak2e(2), ak2f(3), ak2g(3)
  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d, ak4a(2), ak4b(2), ak4c(3), ak4d(3), xk4e, ak4e(2), ak4f(3), ak4g(3)

  integer, parameter :: DK_2_TYPE    = 1
  integer, parameter :: DK_4_TYPE    = 2
  integer, parameter :: UNKNOWN_TYPE = 3

  character(7) :: expName(3) = [character(7):: "dk(2)", "dk(4)", "unknown"]

  xk2a = dk(2)(99)
  xk2b = dk(2)(100)
  xk2c = dk(2)(108)
  xk2d = dk(2)(109)

  xk4a = dk(4)(40000)
  xk4b = dk(4)(40001)
  xk4c = dk(4)(40009)
  xk4d = dk(4)(40010)

  ak2a = [dk(2)(101),dk(2)(102)]
  ak2b = [dk(2)(103),dk(2)(104)]
  ak2c = [dk(2)(105),dk(2)(106),dk(2)(107)]
  ak2d = [dk(2)(110),dk(2)(111),dk(2)(112)]

  ak4a = [dk(4)(40002),dk(4)(40003)]
  ak4b = [dk(4)(40004),dk(4)(40005)]
  ak4c = [dk(4)(40006),dk(4)(40007),dk(4)(40008)]
  ak4d = [dk(4)(40011),dk(4)(40012),dk(4)(40013)]

  xk2e = dk(2)(99) + dk(2)(100)
  ak2e = [dk(2)(101),dk(2)(102)] + [dk(2)(103),dk(2)(104)]
  ak2f = [dk(2)(105),dk(2)(106),dk(2)(107)] + dk(2)(108)
  ak2g = dk(2)(109) + [dk(2)(110),dk(2)(111),dk(2)(112)]
  print *, xk2e, ak2e, ak2f, ak2g

  xk4e = dk(4)(40000) + dk(4)(40001)
  ak4e = [dk(4)(40002),dk(4)(40003)] + [dk(4)(40004),dk(4)(40005)]
  ak4f = [dk(4)(40006),dk(4)(40007),dk(4)(40008)] + dk(4)(40009)
  ak4g = dk(4)(40010) + [dk(4)(40011),dk(4)(40012),dk(4)(40013)]
  print *, xk4e, ak4e, ak4f, ak4g

  xk2e = xk2a + xk2b
  ak2e = ak2a + ak2b
  ak2f = ak2c + xk2c
  ak2g = xk2d + ak2d
  print *, xk2e, ak2e, ak2f, ak2g

  xk4e = xk4a + xk4b
  ak4e = ak4a + ak4b
  ak4f = ak4c + xk4c
  ak4g = xk4d + ak4d
  print *, xk4e, ak4e, ak4f, ak4g

  print *, dk(2)(99) + dk(2)(100)
  print *, [dk(2)(101),dk(2)(102)] + [dk(2)(103),dk(2)(104)]
  print *, [dk(2)(105),dk(2)(106),dk(2)(107)] + dk(2)(108)
  print *, dk(2)(109) + [dk(2)(110),dk(2)(111),dk(2)(112)]

  print *, dk(4)(40000) + dk(4)(40001)
  print *, [dk(4)(40002),dk(4)(40003)] + [dk(4)(40004),dk(4)(40005)]
  print *, [dk(4)(40006),dk(4)(40007),dk(4)(40008)] + dk(4)(40009)
  print *, dk(4)(40010) + [dk(4)(40011),dk(4)(40012),dk(4)(40013)]

  print *, xk2a + xk2b
  print *, ak2a + ak2b
  print *, ak2c + xk2c
  print *, xk2d + ak2d

  print *, xk4a + xk4b
  print *, ak4a + ak4b
  print *, ak4c + xk4c
  print *, xk4d + ak4d

  call scalarTest(dk(2)(99) + dk(2)(100), DK_2_TYPE)
  call arrayTest([dk(2)(101),dk(2)(102)] + [dk(2)(103),dk(2)(104)], DK_2_TYPE)
  call arrayTest([dk(2)(105),dk(2)(106),dk(2)(107)] + dk(2)(108), DK_2_TYPE)
  call arrayTest(dk(2)(109) + [dk(2)(110),dk(2)(111),dk(2)(112)], DK_2_TYPE)

  call scalarTest(dk(4)(40000) + dk(4)(40001), DK_4_TYPE)
  call arrayTest([dk(4)(40002),dk(4)(40003)] + [dk(4)(40004),dk(4)(40005)], DK_4_TYPE)
  call arrayTest([dk(4)(40006),dk(4)(40007),dk(4)(40008)] + dk(4)(40009), DK_4_TYPE)
  call arrayTest(dk(4)(40010) + [dk(4)(40011),dk(4)(40012),dk(4)(40013)], DK_4_TYPE)

  call scalarTest(xk2a + xk2b, DK_2_TYPE)
  call arrayTest(ak2a + ak2b, DK_2_TYPE)
  call arrayTest(ak2c + xk2c, DK_2_TYPE)
  call arrayTest(xk2d + ak2d, DK_2_TYPE)

  call scalarTest(xk4a + xk4b, DK_4_TYPE)
  call arrayTest(ak4a + ak4b, DK_4_TYPE)
  call arrayTest(ak4c + xk4c, DK_4_TYPE)
  call arrayTest(xk4d + ak4d, DK_4_TYPE)

contains

  subroutine scalarTest (arg, expectation)
    class(*), intent(in) :: arg
    integer, intent(in)  :: expectation
    integer :: found
    select type (arg)
    type is (dk(2));    found = DK_2_TYPE;    print *, arg
    type is (dk(4));    found = DK_4_TYPE;    print *, arg
    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", expName(expectation), ", got ", expName(found)
       stop 2
    end if
  end subroutine scalarTest

  subroutine arrayTest (arg, expectation)
    class(*), intent(in) :: arg(:)
    integer, intent(in)  :: expectation
    integer :: found
    select type (arg)
    type is (dk(2));    found = DK_2_TYPE;    print *, arg
    type is (dk(4));    found = DK_4_TYPE;    print *, arg
    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", trim(expName(expectation)), ", got ", trim(expName(found))
       stop 2
    end if
  end subroutine arrayTest

end program dtpUOpSimpleBinaryTKR
