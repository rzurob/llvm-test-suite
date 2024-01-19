!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : pointers to derived types
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Define one type with type-bound procedures taking pointer dummy arguments
!*  and using them, define generic bindings for binary operators.  Verify that the
!*  correct function is invoked when the operation appears as a dummy argument.
!*
!*  Note that only an "Interface" version of this test case exists, since
!*  passed-object dummy arguments cannot be pointers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimplePointerInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  interface operator(+)
     module procedure binaryPlusK2
     module procedure binaryPlusK4
     module procedure binaryPlusK8
  end interface operator(+)

  interface operator(-)
     module procedure unaryMinusK2
     module procedure unaryMinusK4
     module procedure unaryMinusK8
  end interface operator(-)

contains

  type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in), pointer :: this, that
    binaryPlusK2 = dk(2)(this%ivar + that%ivar + 64)
  end function binaryPlusK2

  type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in), pointer :: this, that
    binaryPlusK4 = dk(4)(this%ivar + that%ivar + 32768)
  end function binaryPlusK4

  type(dk(8)) function binaryPlusK8(this,that)
    type(dk(8)), intent(in), pointer :: this, that
    binaryPlusK8 = dk(8)(this%ivar + that%ivar)
  end function binaryPlusK8

  type(dk(2)) function unaryMinusK2(this)
    class(dk(2)), intent(in), pointer :: this
    unaryMinusK2 = dk(2)(int(this%ivar - 65536, 2))
  end function unaryMinusK2

  type(dk(4)) function unaryMinusK4(this)
    class(dk(4)), intent(in), pointer :: this
    unaryMinusK4 = dk(4)(-this%ivar - 8)
  end function unaryMinusK4

  type(dk(8)) function unaryMinusK8(this)
    type(dk(8)), intent(in), pointer :: this
    unaryMinusK8 = dk(8)(-this%ivar)
  end function unaryMinusK8

end module dtpUOpSimplePointerInterfacemod


program dtpUOpSimplePointerInterface

  use dtpUOpSimplePointerInterfacemod
  implicit none

  type(dk(2)),  target  :: xk2a, xk2b
  class(dk(2)), pointer :: xk2ap, xk2bp
  type(dk(4)),  target  :: xk4a, xk4b
  class(dk(4)), pointer :: xk4ap, xk4bp
  type(dk(8)),  target  :: xk8a, xk8b
  type(dk(8)),  pointer :: xk8ap, xk8bp

  integer, parameter :: DK_2_TYPE    = 1
  integer, parameter :: DK_4_TYPE    = 2
  integer, parameter :: DK_8_TYPE    = 3
  integer, parameter :: UNKNOWN_TYPE = 4

  character(7) :: expName(4) = [character(7):: "dk(2)", "dk(4)", "dk(8)", "unknown"]

  xk2a  =  dk(2)(1)
  xk2b  =  dk(2)(2)
  xk2ap => xk2a
  xk2bp => xk2b
  xk4a  =  dk(4)(3)
  xk4b  =  dk(4)(4)
  xk4ap => xk4a
  xk4bp => xk4b
  xk8a  =  dk(8)(123456789987654321_8)
  xk8b  =  dk(8)(876543210012345678_8)
  xk8ap => xk8a
  xk8bp => xk8b


  call test(xk2ap+xk2ap, DK_2_TYPE)
  call test(-xk2bp, DK_2_TYPE)

  call test(xk4ap+xk4ap, DK_4_TYPE)
  call test(-xk4bp, DK_4_TYPE)

  call test(xk8ap+xk8bp, DK_8_TYPE)
  call test(-xk8bp, DK_8_TYPE)

contains

  subroutine test (arg, expectation)
    class(*), intent(in) :: arg
    integer, intent(in)  :: expectation
    integer :: found
    select type (arg)
    type is (dk(2));     found = DK_2_TYPE;     print *, arg
    type is (dk(4));     found = DK_4_TYPE;     print *, arg
    type is (dk(8));     found = DK_8_TYPE;     print *, arg
    class default;       found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", expName(expectation), ", got ", expName(found)
       stop 2
    end if
  end subroutine test

end program dtpUOpSimplePointerInterface
