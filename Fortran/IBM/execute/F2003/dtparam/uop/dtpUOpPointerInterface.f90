!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpPointerInterface
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
!*  Define several types with type-bound procedures taking pointer dummy arguments
!*  and using them, define generic bindings for binary operators.  Verify that the
!*  correct function is invoked when the operation appears as a dummy argument.
!*
!*  Note that only an "Interface" version of this test case exists, since
!*  passed-object dummy arguments cannot be pointers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpPointerInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
  end type dl

  type dkl (k,l)
     integer, kind :: k
     integer, len  :: l
     class(dl(l)), pointer :: dlvar
     class(dk(k)), pointer :: dkvar
  end type dkl

  interface operator(+)
     module procedure binaryPlusK2
     module procedure binaryPlusK4
     module procedure binaryPlusL
     module procedure binaryPlus2L2
     module procedure binaryPlus2L4
  end interface operator(+)

  interface operator(-)
     module procedure unaryMinusK2
     module procedure unaryMinusK4
     module procedure unaryMinusL
     module procedure unaryMinus2L2
     module procedure unaryMinus2L4
  end interface operator(-)

contains

  type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in), pointer :: this, that
    binaryPlusK2 = dk(2)(0)
    if (associated(this) .and. associated(that)) &
         binaryPlusK2 = dk(2)(this%ivar + that%ivar + 64)
  end function binaryPlusK2

  type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in), pointer :: this, that
    binaryPlusK4 = dk(4)(0)
    if (associated(this) .and. associated(that)) &
         binaryPlusK4 = dk(4)(this%ivar + that%ivar + 32768)
  end function binaryPlusK4

  type(dk(2)) function unaryMinusK2(this)
    class(dk(2)), intent(in), pointer :: this
    unaryMinusK2 = dk(2)(0)
    if (associated(this)) unaryMinusK2 = dk(2)(this%ivar - 1000)
  end function unaryMinusK2

  type(dk(4)) function unaryMinusK4(this)
    class(dk(4)), intent(in), pointer :: this
    unaryMinusK4 = dk(4)(0)
    if (associated(this)) unaryMinusK4 = dk(4)(-this%ivar)
  end function unaryMinusK4


  function binaryPlusL(this,that)
    class(dl(*)), intent(in), pointer :: this, that
    type(dl(this%l)) :: binaryPlusL
    binaryPlusL = dl(this%l)('')
    if (associated(this) .and. associated(that)) &
         binaryPlusL = dl(this%l)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32)])
  end function binaryPlusL

  function unaryMinusL(this)
    class(dl(*)), intent(in), pointer :: this
    type(dl(this%l)) :: unaryMinusL
    unaryMinusL = dl(this%l)('')
    if (associated(this)) unaryMinusL = dl(this%l)([this%cvar(this%l:1:-1)])
  end function unaryMinusL


  function binaryPlus2L2(this,that)
    class(dkl(2,*)), intent(in), pointer :: this, that
    type(dkl(2,this%l)) :: binaryPlus2L2
    allocate(binaryPlus2L2%dkvar, source=this%dkvar + that%dkvar)
    allocate(binaryPlus2L2%dlvar, source=(that%dlvar+this%dlvar))
  end function binaryPlus2L2

  function binaryPlus2L4(this,that)
    class(dkl(4,*)), intent(in), pointer :: this, that
    type(dkl(4,this%l)) :: binaryPlus2L4
    allocate(binaryPlus2L4%dlvar, source=this%dlvar + that%dlvar)
    allocate(binaryPlus2L4%dkvar, source=this%dkvar + that%dkvar)
  end function binaryPlus2L4

  function unaryMinus2L2(this)
    class(dkl(2,*)), intent(in), pointer :: this
    type(dkl(2,this%l)) :: unaryMinus2L2
    allocate(unaryMinus2L2%dlvar, source=-this%dlvar)
    allocate(unaryMinus2L2%dkvar, source=-this%dkvar)
  end function unaryMinus2L2

  function unaryMinus2L4(this)
    class(dkl(4,*)), intent(in), pointer :: this
    type(dkl(4,this%l)) :: unaryMinus2L4
    allocate(unaryMinus2L4%dlvar, source=-this%dlvar)
    allocate(unaryMinus2L4%dkvar, source=-this%dkvar)
  end function unaryMinus2L4

end module dtpUOpPointerInterfacemod


program dtpUOpPointerInterface

  use dtpUOpPointerInterfacemod
  implicit none

  type(dk(2)), target  :: xk2a, xk2b, xk2_21, xk2_22
  class(dk(2)), pointer :: xk2ap, xk2bp
  type(dk(4)), target  :: xk4a, xk4b, xk4_23, xk4_24
  class(dk(4)), pointer :: xk4ap, xk4bp

  type(dl(5)), target  :: xla, xlb
  class(dl(5)), pointer :: xlap, xlbp
  class(dl(9)), pointer :: xlbp1
  type(dl(7)), target  :: dl7
  type(dl(9)), target  :: dl9a, dl9b, dl9c

  type(dkl(2,9)), target  :: xkl2a, xkl2b
  class(dkl(2,9)), pointer :: xkl2ap, xkl2bp
  type(dkl(4,7)), target  :: xkl4a, xkl4b
  class(dkl(4,7)), pointer :: xkl4ap, xkl4bp

  integer, parameter :: DK_2_TYPE    = 1
  integer, parameter :: DK_4_TYPE    = 2
  integer, parameter :: DL_TYPE      = 3
  integer, parameter :: DKL_2_TYPE  = 4
  integer, parameter :: DKL_4_TYPE  = 5
  integer, parameter :: UNKNOWN_TYPE = 6

  character(8) :: expName(6) = [character(8):: "dk(2)", "dk(4)", "dl(*)", "dkl(2,*)", "dkl(4,*)", "unknown"]

  xk2a  =  dk(2)(1)
  xk2b  =  dk(2)(2)
  xk2_21 =  dk(2)(21)
  xk2_22 =  dk(2)(22)
  xk2ap => xk2a
  xk2bp => xk2b

  xk4a  =  dk(4)(3)
  xk4b  =  dk(4)(4)
  xk4_23 =  dk(4)(23)
  xk4_24 =  dk(4)(24)
  xk4ap => xk4a
  xk4bp => xk4b

  xla  =  dl(5)(['a','b','c','d','e'])
  xlb  =  dl(5)(['a','b','c','d','e'])
  xlap => xla
  xlbp => xlb

  dl9a   =  dl(9)(['a','b','c','d','e','f','g','h','i'])
  dl9b   =  dl(9)(['j','i','h','g','f','e','d','c','b'])
  dl9c   =  dl(9)(['a','a','b','b','c','c','d','d','z'])
  dl7    =  dl(7)(['c','d','e','f','g','h','i'])


  xkl2a  =  dkl(2,9)(dl9a,xk2_21)
  xkl2b  =  dkl(2,9)(dl9b,xk2_22)
  xkl2ap => xkl2a
  xkl2bp => xkl2b

  xkl4a  =  dkl(4,7)(dl7,xk4_23)
  xkl4b  =  dkl(4,7)(dl7,xk4_24)
  xkl4ap => xkl4a
  xkl4bp => xkl4b

  ! Verify that "test" is working:
  call test(xk2ap, DK_2_TYPE)
  call test(xk4ap, DK_4_TYPE)
  call test(xlap, DL_TYPE)
  call test(xkl2ap, DKL_2_TYPE)
  call test(xkl4ap, DKL_4_TYPE)

  ! Now for the real tests:
  call test(xk2ap+xk2bp, DK_2_TYPE)
  call test(-xk2bp, DK_2_TYPE)

  call test(xk4ap+xk4bp, DK_4_TYPE)
  call test(-xk4bp, DK_4_TYPE)

  call test(xlap+xlbp, DL_TYPE)
  call test(-xlbp, DL_TYPE)

  call test(xkl2ap+xkl2bp, DKL_2_TYPE)
  call test(-xkl2bp, DKL_2_TYPE)

  call test(xkl4ap+xkl4bp, DKL_4_TYPE)
  call test(-xkl4bp, DKL_4_TYPE)

  xlbp1 => dl9c
  call test(xkl2ap%dlvar+xlbp1, DL_TYPE)
  call test(-xkl2bp%dlvar, DL_TYPE)

  call test(xlbp1+xkl2ap%dlvar, DL_TYPE)

  call test(xkl4ap%dkvar + xkl4bp%dkvar, DK_4_TYPE)
  call test(-xkl4bp%dkvar, DK_4_TYPE)

contains

  recursive subroutine test (arg, expectation)
    class(*), intent(in) :: arg
    integer, intent(in)  :: expectation
    integer :: found
    select type (arg)
    type is (dk(2));    found = DK_2_TYPE;    print *, trim(expName(found)), ": ", arg
    type is (dk(4));    found = DK_4_TYPE;    print *, trim(expName(found)), ": ", arg
    type is (dl(*));    found = DL_TYPE;      print *, trim(expName(found)), ": ", arg%l, arg
    type is (dkl(2,*)); found = DKL_2_TYPE;   print *, trim(expName(found)), ": ", arg%l, ", dl/dk are:"
                                              call test(arg%dlvar, DL_TYPE)
                                              call test(arg%dkvar, DK_2_TYPE)
    type is (dkl(4,*)); found = DKL_4_TYPE;   print *, trim(expName(found)), ": ", arg%l, ", dl/dk are:"
                                              call test(arg%dlvar, DL_TYPE)
                                              call test(arg%dkvar, DK_4_TYPE)
    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", trim(expName(expectation)), ", got ", trim(expName(found))
       stop 2
    end if
  end subroutine test

end program dtpUOpPointerInterface
