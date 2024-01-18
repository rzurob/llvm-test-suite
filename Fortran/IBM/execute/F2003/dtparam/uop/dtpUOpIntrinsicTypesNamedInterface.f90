!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpIntrinsicTypesNamedInterface
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : involvement of intrinsic types and named operators (generic interface)
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
!*  Define a type with named binary operators associated via generic interfaces and
!*  verify correct associativity, including intrinsic types.
!*  Here we use unary and binary operators, so precedence does play a small role:
!*  named binary operators all have the same low precedence (the lowest of all
!*  operators), while named unary operators all have the same high precedence
!*  (the highest of all operators).
!*  We have to be careful with expressions where different levels of precedence
!*  are possible, e.g., in the expression a*b*c, only one order is possible: a*b
!*  followed by ...*c (ditto c+a*b -> a*b is followed by c+...), but in the
!*  expression a*b+c*d, either a*b or c*d can be computed first.
!*  In this program, a.addit.(.neg.b) and a.addit.b.addit.c can each be evaluated
!*  only one way, but in (.neg.a).addit.(.neg.b), either (.neg.a) or (.neg.b) can be first.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpIntrinsicTypesNamedInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  interface operator(.addit.)
     module procedure binaryPlusK
     module procedure binaryPlus
  end interface operator(.addit.)

  interface operator(.subit.)
     module procedure binaryMinusK
     module procedure binaryMinus
  end interface operator(.subit.)

  interface operator(.neg.)
     module procedure unaryNegK
     module procedure unaryNeg
  end interface operator(.neg.)

  character(1), parameter :: PLUS   = '+'
  character(1), parameter :: PLUSK  = 'p'
  character(1), parameter :: MINUS  = '-'
  character(1), parameter :: MINUSK = 'm'
  character(1), parameter :: NEGATE = '#'
  character(1), parameter :: NEGATEK= 'N'

  type event
     character(1) :: op = ' '
     integer :: left = 0
     integer :: right = 0
     integer :: outcome = 0
   contains
     generic :: operator (==) => eventEq
     procedure, pass :: eventEq
  end type event

  integer, parameter :: MAXDEPTH = 10
  type(event), save :: events(MAXDEPTH)
  integer, save :: top = 0

contains

  subroutine resetEvents
    events = event()
    top = 0
  end subroutine resetEvents

  subroutine testEvents(test,expected)
    character(*), intent(in) :: test
    type(event), intent(in) :: expected(:)
    if (.not.eventsAsExpected(expected)) call dumpEventsAndExit(test, expected)
  end subroutine testEvents

  logical function eventsAsExpected(expected)
    type(event), intent(in) :: expected(:)
    integer :: i
    eventsAsExpected = size(expected) == top
    i = 1
    do while (eventsAsExpected .and. i <= top)
       eventsAsExpected = events(i)==expected(i)
       i = i + 1
    end do
  end function eventsAsExpected

  subroutine dumpEventsAndExit(test, expected)
    character(*), intent(in) :: test
    type(event), intent(in) :: expected(:)
    call dumpEvents(test, expected)
    stop 2
  end subroutine dumpEventsAndExit

  subroutine dumpEvents(test, expected)
    character(*), intent(in) :: test
    type(event), intent(in) :: expected(:)
    print *, "At ", test
    print *, " recorded:", events
    print *, " expected:", expected
  end subroutine dumpEvents


  logical function eventEq(e1, e2)
    class(event), intent(in) :: e1, e2
    eventEq = e1%op == e1%op .and. e2%left == e2%left .and. e2%right == e2%right .and. e2%outcome == e2%outcome
  end function eventEq

  subroutine recordEvent(op, left, right, outcome)
     character(1), intent(in) :: op
     integer, intent(in) :: left, right, outcome
     if (top < MAXDEPTH) then
        top = top + 1
        events(top) = event(op, left, right, outcome)
     else
        print *, "Expression too deep: ", op, left, right, outcome
     end if
   end subroutine recordEvent


  type(dk(4)) function binaryPlusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK = dk(4)(this%ivar + that%ivar)
    call recordEvent(PLUSK, this%ivar, that%ivar, binaryPlusK%ivar)
  end function binaryPlusK

  type(dk(4)) function binaryPlus(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryPlus = dk(4)(this%ivar + that)
    call recordEvent(PLUS, this%ivar, that, binaryPlus%ivar)
  end function binaryPlus

  type(dk(4)) function binaryMinusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryMinusK = dk(4)(this%ivar - that%ivar)
    call recordEvent(MINUSK, this%ivar, that%ivar, binaryMinusK%ivar)
  end function binaryMinusK

  type(dk(4)) function binaryMinus(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryMinus = dk(4)(this%ivar - that)
    call recordEvent(MINUS, this%ivar, that, binaryMinus%ivar)
  end function binaryMinus

  type(dk(4)) function unaryNegK(this)
    class(dk(4)), intent(in) :: this
    unaryNegK = dk(4)(-this%ivar)
    call recordEvent(NEGATEK, this%ivar, 0, unaryNegK%ivar)
  end function unaryNegK

  type(dk(4)) function unaryNeg(this)
    integer, intent(in) :: this
    unaryNeg = dk(4)(-this)
    call recordEvent(NEGATE, this, 0, unaryNeg%ivar)
  end function unaryNeg

end module dtpUOpIntrinsicTypesNamedInterfacemod


program dtpUOpIntrinsicTypesNamedInterface

  use dtpUOpIntrinsicTypesNamedInterfacemod
  implicit none

  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d

  xk4a   = dk(4)(12)
  xk4b   = dk(4)(2)
  xk4c   = dk(4)(3)

  call resetEvents
  xk4d = xk4a .addit. xk4b
  call testEvents("xk4d = xk4a .addit. xk4b", [event(PLUSK,12,2,14)])

  call resetEvents
  xk4d = xk4a .addit. 2 .addit. xk4b
  call testEvents("xk4d = xk4a .addit. 2 .addit. xk4b", [event(PLUS,12,2,14), event(PLUSK,14,2,16)])
  
  call resetEvents
  xk4d = ((xk4a .addit. 2) .addit. xk4b)
  call testEvents("xk4d = ((xk4a .addit. 2) .addit. xk4b)", [event(PLUS,12,2,14), event(PLUSK,14,2,16)])

  call resetEvents
  xk4d = xk4a .subit. xk4c
  call testEvents("xk4d = xk4a .subit. xk4c", [event(MINUSK,12,3,9)])

  call resetEvents
  xk4d = (xk4a .subit. xk4b .subit. 77)
  call testEvents("xk4d = (xk4a .subit. xk4b .subit. 77)", [event(MINUSK,12,2,10), event(MINUS,10,77,-67)])

  call resetEvents
  xk4d = xk4a .addit. 5 .addit. xk4c .addit. 55 .addit. 7
  call testEvents("xk4d = xk4a .addit. 5 .addit. xk4c .addit. 55 .addit. 7", &
                  [event(PLUS,12,5,17), event(PLUSK,17,3,20), event(PLUS,20,55,75), event(PLUS,75,7,82)])

  call resetEvents
  xk4d = (((xk4a .addit. 5) .addit. xk4c) .addit. 55) .addit. 7
  call testEvents("xk4d = (((xk4a .addit. 5) .addit. xk4c) .addit. 55) .addit. 7", &
                  [event(PLUS,12,5,17), event(PLUSK,17,3,20), event(PLUS,20,55,75), event(PLUS,75,7,82)])

  call resetEvents
  xk4d = xk4a .addit. 66 .addit. xk4b .addit. 44
  call testEvents("xk4d = xk4a .addit. 66 .addit. xk4b .addit. 44", [event(PLUS,12,66,78), event(PLUSK,78,2,80), event(PLUS,80,44,124)])

  call resetEvents
  xk4d = xk4a .subit. .neg. xk4c
  call testEvents("xk4d = xk4a .subit. .neg. xk4c", [event(NEGATEK,3,0,-3), event(MINUSK,12,-3,15)])

  call resetEvents
  xk4d = xk4a .subit. (.neg. xk4c)
  call testEvents("xk4d = xk4a .subit. (.neg. xk4c)", [event(NEGATEK,3,0,-3),event(MINUSK,12,-3,15)])

  call resetEvents
  xk4d = xk4a .subit. .neg. xk4c .addit. xk4b
  call testEvents("xk4d = xk4a .subit. .neg. xk4c .addit. xk4b", [event(NEGATEK,3,0,-3), event(MINUSK,12,-3,15), event(PLUSK,15,2,17)])

  call resetEvents
  xk4d = xk4a .subit. .neg. xk4c .addit. .neg. (.neg. 99)
  ! this is nasty: there are six valid sequences of evaluation - if none of the first five work, let it die on the sixth one if it also fails:
  if (.not. eventsAsExpected([event(NEGATE,99,0,-99), event(NEGATEK,3,0,-3), event(NEGATEK,-99,99), event(MINUSK,12,-3,15), event(PLUSK,15,99,114)]) &
      .and. .not. eventsAsExpected([event(NEGATE,99,0,-99), event(NEGATEK,3,0,-3), event(MINUSK,12,-3,15), event(NEGATEK,-99,99), event(PLUSK,15,99,114)]) &
      .and. .not. eventsAsExpected([event(NEGATE,99,0,-99), event(NEGATEK,-99,0,99), event(NEGATEK,3,0,-3), event(MINUSK,12,-3,15), event(PLUSK,15,99,114)]) &
      .and. .not. eventsAsExpected([event(NEGATEK,3,0,-3), event(NEGATE,99,0,-99), event(NEGATEK,-99,0,99), event(MINUSK,12,-3,15), event(PLUSK,15,99,114)]) &
      .and. .not. eventsAsExpected([event(NEGATEK,3,0,-3), event(NEGATE,99,0,-99), event(MINUSK,12,-3,15), event(NEGATEK,-99,0,99), event(PLUSK,15,99,114)])) &
      call testEvents("xk4d = xk4a .subit. .neg. xk4c .addit. .neg. (.neg. 99)", &
                      [event(NEGATEK,3,0,-3), event(MINUSK,12,-3,15), event(NEGATE,99,0,-99), event(NEGATEK,-99,0,99), event(PLUSK,15,99,114)])
  print *, "done"

end program dtpUOpIntrinsicTypesNamedInterface
