!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpPrecedenceInterface
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct pairings of operators and operands, given precedence - all same type (generic interface)
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
!*  Define several types with procedures for binary operators associated via
!*  generic interfaces, and verify correct precedence.
!*  We want to be careful to make sure all the correct procedures are invoked
!*  in the correct order.  Because a*b+b*c has two possible valid orders (either
!*  product can be computed first, as long as both are computed before the sum),
!*  we need to create a trace of what we're doing.  There are several expressions
!*  with multiple valid orders, so we could have dozens of valid output sequences
!*  to check, which means we can't just print out a trace and compare the output
!*  with a verification file or two.  That means we need to compare possible traces
!*  inside the program, so we create the routines to record events (recordKEvent,
!*  recordLEvent), test events (eventsAsExpected, testEvents), reset the list of
!*  recorded events (resetEvents) and display errors if we discover them (dumpEvents).
!*  eventsAsExpected returns TRUE if the recorded events match a given sequence,
!*  and otherwise FALSE.  testEvents exits the program with a return code of 2,
!*  after dumping the event trace, if the sequences do not match.  To test a single
!*  valid sequence, we just call testEvents.  To test multiple valid sequences,
!*  we test all but one via eventsAsExpected, invoking testEvents on the remaining
!*  sequence only if there are no matches.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpPrecedenceInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
  end type d2l

  interface operator(*)
    module procedure binaryStar2L
    module procedure binaryStarK
  end interface operator(*)

  interface operator(**)
    module procedure binaryPower2L
    module procedure binaryPowerK
  end interface operator(**)

  interface operator(+)
    module procedure binaryPlus2L
    module procedure binaryPlusK
  end interface operator(+)

  interface operator(-)
    module procedure binaryMinus2L
    module procedure binaryMinusK
  end interface operator(-)

  interface operator(/)
    module procedure binarySlash2L
    module procedure binarySlashK
  end interface operator(/)

  character(1), parameter :: PLUSK  = 'p'
  character(1), parameter :: MINUSK = 'm'
  character(1), parameter :: STARK  = 's'
  character(1), parameter :: SLASHK = 'd'
  character(1), parameter :: POWERK = 'e'

  character(1), parameter :: PLUSL  = '+'
  character(1), parameter :: MINUSL = '-'
  character(1), parameter :: STARL  = '*'
  character(1), parameter :: SLASHL = '/'
  character(1), parameter :: POWERL = '^'

  integer, parameter :: MAXDEPTH  = 10
  integer, parameter :: MAXLENGTH = 60
  character(MAXLENGTH), save :: events(MAXDEPTH)
  integer, save :: top = 0

contains

  subroutine resetEvents
    events = ''
    top = 0
  end subroutine resetEvents

  subroutine testEvents(test,expected)
    character(*), intent(in) :: test
    character(MAXLENGTH), intent(in) :: expected(:)
    if (.not.eventsAsExpected(expected)) call dumpEventsAndExit(test, expected)
  end subroutine testEvents

  logical function eventsAsExpected(expected)
    character(MAXLENGTH), intent(in) :: expected(:)
    eventsAsExpected = size(expected) == top
    eventsAsExpected = eventsAsExpected .and. all(events(1:top) == expected(1:top))
  end function eventsAsExpected

  subroutine dumpEventsAndExit(test, expected)
    character(*), intent(in) :: test
    character(MAXLENGTH), intent(in) :: expected(:)
    call dumpEvents(test, expected)
    stop 2
  end subroutine dumpEventsAndExit

  subroutine dumpEvents(test, expected)
    character(*), intent(in) :: test
    character(MAXLENGTH), intent(in) :: expected(:)
    integer :: i
    print *, "At ", test
    print *, "recorded:"
    do i=1,top
       print *, "  ", trim(events(i))
    end do
    print *, "expected:"
    do i=1,top
       print *, "  ", trim(expected(i))
    end do
  end subroutine dumpEvents
  

  subroutine recordKEvent(op, left, right, outcome)
     character(1), intent(in) :: op
     type(dk(4)), intent(in) :: left, right, outcome
     if (top < MAXDEPTH) then
        top = top + 1
        write(events(top),*) op, left%ivar, right%ivar, "->", outcome%ivar
     else
        print *, "Expression too deep: ", op, left%ivar, right%ivar, "->", outcome%ivar
     end if
   end subroutine recordKEvent


  subroutine recordLEvent(op, left, right, outcome)
     character(1), intent(in) :: op
     type(d2l(4,*)), intent(in) :: left, right, outcome
     if (top < MAXDEPTH) then
        top = top + 1
        write(events(top),*) op, left%l, left%cvar, left%ivar, right%l, right%cvar, right%ivar, "->", outcome%l, outcome%cvar, outcome%ivar
     else
        print *, "Expression too deep: ", op, left, right, outcome
     end if
   end subroutine recordLEvent


  type(dk(4)) function binaryPlusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK = dk(4)(this%ivar + that%ivar)
    call recordKEvent(PLUSK, this, that, binaryPlusK)
  end function binaryPlusK

  type(dk(4)) function binaryMinusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryMinusK = dk(4)(this%ivar - that%ivar)
    call recordKEvent(MINUSK, this, that, binaryMinusK)
  end function binaryMinusK

  type(dk(4)) function binaryStarK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryStarK = dk(4)(this%ivar * that%ivar)
    call recordKEvent(STARK, this, that, binaryStarK)
  end function binaryStarK

  type(dk(4)) function binarySlashK(this,that)
    class(dk(4)), intent(in) :: this, that
    binarySlashK = dk(4)(this%ivar / that%ivar)
    call recordKEvent(SLASHK, this, that, binarySlashK)
  end function binarySlashK

  type(dk(4)) function binaryPowerK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPowerK = dk(4)(this%ivar ** that%ivar)
    call recordKEvent(POWERK, this, that, binaryPowerK)
  end function binaryPowerK


  function binaryPlus2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+1)) :: binaryPlus2L
    binaryPlus2L = d2l(4,this%l+1)([achar(abs(mod(iachar(this%cvar) + iachar(that%cvar), 96))+32), 'Y'],this%ivar + that%ivar)
    call recordLEvent(PLUSL, this, that, binaryPlus2L)
  end function binaryPlus2L

  function binaryMinus2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-1)) :: binaryMinus2L
    binaryMinus2L = d2l(4,this%l-1)(['Q', achar(abs(mod(iachar(this%cvar(1:this%l-2)) - iachar(that%cvar(1:that%l-2)), 96))+32)],this%ivar - that%ivar)
    call recordLEvent(MINUSL, this, that, binaryMinus2L)
  end function binaryMinus2L

  function binaryStar2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+2)) :: binaryStar2L
    binaryStar2L = d2l(4,this%l+2)([achar(abs(mod(iachar(this%cvar) * iachar(that%cvar), 96))+32), 'X', 'W'],this%ivar * that%ivar)
    call recordLEvent(STARL, this, that, binaryStar2L)
  end function binaryStar2L

  function binarySlash2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-2)) :: binarySlash2L
    binarySlash2L = d2l(4,this%l-2)(['P', achar(abs(mod(iachar(this%cvar(1:this%l-3)) / (mod(iachar(that%cvar(1:that%l-3)),8)+1), 96))+32)],this%ivar / that%ivar)
    call recordLEvent(SLASHL, this, that, binarySlash2L)
  end function binarySlash2L

  function binaryPower2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+3)) :: binaryPower2L
    binaryPower2L = d2l(4,this%l+3)([achar(abs(mod(iachar(this%cvar) ** mod(iachar(that%cvar),3), 96))+32), 'V', 'U', 'T'],this%ivar ** that%ivar)
    call recordLEvent(POWERL, this, that, binaryPower2L)
  end function binaryPower2L


end module dtpUOpPrecedenceInterfacemod


program dtpUOpPrecedenceInterface

  use dtpUOpPrecedenceInterfacemod
  implicit none

  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d

  type(d2l(4,7)) :: x2l4a
  type(d2l(4,5)) :: x2l4b, x2l4c
  type(d2l(4,8)) :: x2l4d, x2l4j
  type(d2l(4,6)) :: x2l4e
  type(d2l(4,3)) :: x2l4f, x2l4g
  type(d2l(4,4)) :: x2l4h, x2l4i
  type(d2l(4,9)) :: x2l4k

  xk4a   = dk(4)(12)
  xk4b   = dk(4)(2)
  xk4c   = dk(4)(3)

  x2l4a  = d2l(4,7)(['a','u','v','m','n','w','x'],13)
  x2l4b  = d2l(4,5)(['b','t','w','l','o'],7)
  x2l4c  = d2l(4,5)(['c','s','x','k','p'],4)
  x2l4e  = d2l(4,6)(['d','r','y','j','q','v'],9)
  x2l4f  = d2l(4,3)(['e','q','z'],2)
  x2l4g  = d2l(4,3)(['f','o','a'],5)
  x2l4h  = d2l(4,4)(['g','n','b','i'],69)
  x2l4i  = d2l(4,4)(['h','m','c','h'],23)
  x2l4j  = d2l(4,8)(['i','l','d','g','r','u','y','a'],122)

  call resetEvents
  xk4d  = xk4a+xk4b*xk4c   ! 12+2*3 = 12 + 6 = 18
  call testEvents("xk4d  = xk4a+xk4b*xk4c", [character(MAXLENGTH):: " s 2 3 -> 6", " p 12 6 -> 18"])

  call resetEvents
  xk4d  = xk4a+(xk4b*xk4c) ! 12+(2*3) = 12 + 6 = 18
  call testEvents("xk4d  = xk4a+(xk4b*xk4c)", [character(MAXLENGTH):: " s 2 3 -> 6", " p 12 6 -> 18"])

  call resetEvents
  xk4d  = xk4a*xk4b+xk4c   ! 12*2+3 = 24 + 3 = 27
  call testEvents("xk4d  = xk4a*xk4b+xk4c", [character(MAXLENGTH):: " s 12 2 -> 24", " p 24 3 -> 27"])

  call resetEvents
  xk4d  = (xk4a*xk4b)+xk4c ! (12*2)+3 = 24 + 3 = 27
  call testEvents("xk4d  = (xk4a*xk4b)+xk4c", [character(MAXLENGTH):: " s 12 2 -> 24", " p 24 3 -> 27"])

  call resetEvents
  xk4d  = (xk4a+xk4b)*xk4c ! (12+2) * 3 = 14 * 3 = 42
  call testEvents("xk4d  = (xk4a+xk4b)*xk4c", [character(MAXLENGTH):: " p 12 2 -> 14", " s 14 3 -> 42"])

  call resetEvents
  xk4d  = (xk4a-xk4b)*xk4c ! (12-2) * 3 = 10 * 3 = 30
  call testEvents("xk4d  = (xk4a-xk4b)*xk4c", [character(MAXLENGTH):: " m 12 2 -> 10", " s 10 3 -> 30"])

  call resetEvents
  xk4d  = xk4a*xk4b+xk4b*xk4c ! 12*2 + 2*3 = 24 + 2*3 (or 12*2 + 6) = 24 + 6 = 30
  ! two possible orders: 1. (xk4a*xk4b) then (xk4b*xk4c) then "+", or 2. (xk4b*xk4c) then (xk4a*xk4b) then "+"
  if (.not. eventsAsExpected([character(MAXLENGTH):: " s 12 2 -> 24", " s 2 3 -> 6", " p 24 6 -> 30"])) &
       call testEvents("xk4d  = xk4a*xk4b+xk4b*xk4c", [character(MAXLENGTH):: " s 2 3 -> 6", " s 12 2 -> 24", " p 24 6 -> 30"])

  call resetEvents
  xk4d  = xk4a/xk4b*xk4b**xk4c-xk4c ! 12/2*2**3-3 = 12/2*8-3 = 6*8-3 = 48-3 = 45
  ! two possible orders: 1. xk4b**xk4c then xk4a/xk4b then ...*... then ...-xk4c, or 2. xk4a/xk4b then xk4b**xk4c then  ...*... then ...-xk4c
  if (.not. eventsAsExpected([character(MAXLENGTH):: " d 12 2 -> 6", " e 2 3 -> 8", " s 6 8 -> 48", " m 48 3 -> 45"])) &
       call testEvents("xk4d  = xk4a/xk4b*xk4b**xk4c-xk4c", &
                       [character(MAXLENGTH):: " e 2 3 -> 8", " d 12 2 -> 6", " s 6 8 -> 48", " m 48 3 -> 45"])

  call resetEvents
  x2l4d  = x2l4a + x2l4b*x2l4c   ! 13 + 7 * 4 = 13 + 28 = 41
  call testEvents("x2l4d  = x2l4a + x2l4b*x2l4c", &
                  [character(MAXLENGTH):: " * 5 btwlo 7 5 csxkp 4 -> 7 &|hDPXW 28", " + 7 auvmnwx 13 7 &|hDPXW 28 -> 8 GQ>q~//Y 41"])

  call resetEvents
  x2l4d  = x2l4a + (x2l4b*x2l4c) ! 13 + 7 * 4 = 13 + 28 = 41
  call testEvents("x2l4d  = x2l4a + (x2l4b*x2l4c)", &
                  [character(MAXLENGTH):: " * 5 btwlo 7 5 csxkp 4 -> 7 &|hDPXW 28", " + 7 auvmnwx 13 7 &|hDPXW 28 -> 8 GQ>q~//Y 41"])

  call resetEvents
  x2l4d  = x2l4b*x2l4c + x2l4a   ! 7*4 + 13 = 28 + 13 = 41
  call testEvents("x2l4d  = x2l4b*x2l4c + x2l4a", &
                  [character(MAXLENGTH):: " * 5 btwlo 7 5 csxkp 4 -> 7 &|hDPXW 28", " + 7 &|hDPXW 28 7 auvmnwx 13 -> 8 GQ>q~//Y 41"])

  call resetEvents
  x2l4d  = (x2l4c*x2l4b) + x2l4a ! (4*7)+13 = 28 + 13 = 41
  call testEvents("x2l4d  = (x2l4c*x2l4b) + x2l4a", &
                  [character(MAXLENGTH):: " * 5 csxkp 4 5 btwlo 7 -> 7 &|hDPXW 28", " + 7 &|hDPXW 28 7 auvmnwx 13 -> 8 GQ>q~//Y 41"])

  call resetEvents
  x2l4d  = (x2l4b+x2l4c) * x2l4e ! (7+4)*9 = 11 * 9 = 99
  call testEvents("x2l4d  = (x2l4b+x2l4c) * x2l4e", &
                  [character(MAXLENGTH):: " + 5 btwlo 7 5 csxkp 4 -> 6 %GO7?Y 11", " * 6 %GO7?Y 11 6 dryjqv 9 -> 8 T>Wf/FXW 99"])

  call resetEvents
  x2l4d  = (x2l4b-x2l4c) * x2l4e ! (7-4)*9 = 3 * 9 = 27
  call testEvents("x2l4d  = (x2l4b-x2l4c) * x2l4e", &
                  [character(MAXLENGTH):: " - 5 btwlo 7 5 csxkp 4 -> 4 Q!!! 3", " * 4 Q!!! 3 6 dryjqv 9 -> 6 D2YJXW 27"])

  call resetEvents
  x2l4d  = x2l4b*x2l4c + x2l4c*x2l4b ! 7*4 + 4*7 = 28 + 4*7 (or 7*4 + 28) = 28 + 28 = 56
  ! two possible orders: 1. (x2l4b*x2l4c) then (x2l4c*x2l4b) then "+", or 2. (x2l4c*x2l4b) then (x2l4b*x2l4c) then "+"
  if (.not. eventsAsExpected([character(MAXLENGTH):: " * 5 btwlo 7 5 csxkp 4 -> 7 &|hDPXW 28", " * 5 csxkp 4 5 btwlo 7 -> 7 &|hDPXW 28", &
                                                     " + 7 &|hDPXW 28 7 &|hDPXW 28 -> 8 lX0H`pnY 56"])) &
      call testEvents("x2l4d  = x2l4b*x2l4c + x2l4c*x2l4b", &
                      [character(MAXLENGTH):: " * 5 csxkp 4 5 btwlo 7 -> 7 &|hDPXW 28", " * 5 btwlo 7 5 csxkp 4 -> 7 &|hDPXW 28", &
                                              " + 7 &|hDPXW 28 7 &|hDPXW 28 -> 8 lX0H`pnY 56"])

  call resetEvents
  x2l4k  = x2l4h/x2l4i*x2l4f**x2l4g - x2l4j ! 69/23*2**5-122 = 69/23*32-122 = 3*32-122 = 96-122 = -26
  ! two possible orders: 1. x2l4f**x2l4g then x2l4h/x2l4i then ...*... then ...-x2l4j, or 2. x2l4h/x2l4i then x2l4f**x2l4g then  ...*... then ...-x2l4j
  if (.not. eventsAsExpected([character(MAXLENGTH):: " / 4 gnbi 69 4 hmch 23 -> 2 P' 3", " ^ 3 eqz 2 3 foa 5 -> 6 !!:VUT 32", &
                                                     " * 2 P' 3 6 !!:VUT 32 -> 4 PGXW 96", " - 4 PGXW 96 8 ildgruya 122 -> 3 Q9E -26"])) &
      call testEvents("x2l4k  = x2l4h/x2l4i*x2l4f**x2l4g - x2l4j", &
                      [character(MAXLENGTH):: " ^ 3 eqz 2 3 foa 5 -> 6 !!:VUT 32", " / 4 gnbi 69 4 hmch 23 -> 2 P' 3", &
                                              " * 2 P' 3 6 !!:VUT 32 -> 4 PGXW 96", " - 4 PGXW 96 8 ildgruya 122 -> 3 Q9E -26"])

  print *, "end"

end program dtpUOpPrecedenceInterface
