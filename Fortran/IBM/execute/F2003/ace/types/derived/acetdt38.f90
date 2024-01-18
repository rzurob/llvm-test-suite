!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt38
!*
!*  DATE                       : 2006-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC in I/O, as output-item, using DTIO, incl. polymorphism
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : DTIO, polymorphism
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create derived types with user-defined DTIO, and print AC's with these out.
!*  Make sure that they exhibit polymorphism
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt38mod

  implicit none

  type Base
     integer :: ival
   contains
     procedure :: printItem => basePrint
     generic :: write(formatted) => printItem
  end type Base

  type, extends(Base) :: Derived
     character :: cval
   contains
     procedure :: printItem => derivedPrint
  end type Derived

  type, extends(Base) :: Derived2
     logical :: lval
   contains
     procedure :: printItem => derived2Print
  end type Derived2

  type, extends(Base) :: Derived3
     character(3) :: c3val
  end type Derived3

contains

  subroutine basePrint(this,unit,iotype,vlist,iostat,iomsg)
    class(Base), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Base[',i0,']')", iostat=iostat) this % ival
  end subroutine basePrint

  subroutine derivedPrint(this,unit,iotype,vlist,iostat,iomsg)
    class(Derived), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Derived[',i0,',',a,']')", iostat=iostat) this % ival, this % cval
  end subroutine derivedPrint

  subroutine derived2Print(this,unit,iotype,vlist,iostat,iomsg)
    class(Derived2), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Derived2[',i0,',',l1,']')", iostat=iostat) this % ival, this % lval
  end subroutine derived2Print

end module acetdt38mod


program acetdt38

  use acetdt38mod
  implicit none

  type (Base), target       :: b, bt
  type (Derived), target    :: d, dt
  type (Derived2), target   :: d2, d2t
  type (Derived3), target   :: d3, d3t

  class (Base), pointer     :: bp
  class (Derived), pointer  :: dp
  class (Derived2), pointer :: d2p
  class (Derived3), pointer :: d3p

  b  = base(1)
  d  = derived(2,'a')
  d2 = derived2(3,.true.)
  d3 = derived3(4,'abc')

  bt  = base(-1)
  dt  = derived(-2,'X')
  d2t = derived2(-3,.FALSE.)
  d3t = derived3(-4,'XXX')

  print *, 'L 1:', b
  print *, 'L 2:', d
  print *, 'L 3:', d2
  print *, 'L 4:', d3

  ! The easy stuff: print known types with and without type specifiers:
  print *, 'L 5:', [Base:: b, d % Base, d2 % Base, d3 % Base], &
                   [b,  d % Base, d2 % Base, d3 % Base]
  print *, 'L 6:', [Derived::  d,  derived(101,'b'), derived(base=b,cval='c')],          &
                   [d,  derived(102,'d'), derived(base=b,cval='e')]
  print *, 'L 7:', [Derived2:: d2, derived2(103,.false.), derived2(base=b,lval=.true.)], &
                   [d2, derived2(104,.false.), derived2(base=b,lval=.false.)]
  print *, 'L 8:', [Derived3:: d3, derived3(105,'bcd'), derived3(base=b,c3val='def')],    &
                   [d3, derived3(106,'efg'), derived3(base=b,c3val='fgh')]

  ! Try polymorphic pointers referencing the most basic type they can:
  bp  => b
  dp  => d
  d2p => d2
  d3p => d3
  print *, 'L 9:', [Base:: bp, dp % Base, d2p % Base, d3p % Base]
  print *, 'L10:', [Derived::  dp]
  print *, 'L11:', [Derived2:: d2p]
  print *, 'L12:', [Derived3:: d3p]

  ! Repeat with anything more derived:
  bp  => d
  print *, 'L13:', [bp]
  bp  => d2
  print *, 'L14:', [bp]
  bp  => d3
  print *, 'L15:', [bp]

  bp  => d
  print *, 'L16:', [Base:: bp]
  bp  => d2
  print *, 'L17:', [Base:: bp]
  bp  => d3
  print *, 'L18:', [Base:: bp]

  ! These work because rainbow returns a reference to a Base DT which
  ! has user-defined DTIO, even though the reference is polymorphic.

  print *, 'L19:', rainbow(5,2), rainbow(6,'a'), rainbow(7,.true.), rainbow(8,'def')
  print *, 'L20:', [rainbow(13,2), rainbow(14,'a'), rainbow(15,.true.), rainbow(16,'def')]
  print *, 'L21:', [Base:: rainbow(9,2), rainbow(10,'a'), rainbow(11,.true.), rainbow(12,'def')]
  print *, 'L22:', [rainbow(17,2)]
  print *, 'L23:', [rainbow(18,'a')]
  print *, 'L24:', [rainbow(19,.true.)]
  print *, 'L25:', [rainbow(20,'def')]

contains

  function rainbow(a1, a2)
    integer   :: a1
    class(*)  :: a2
    class(Base), pointer :: rainbow

    select type(a2)
    type is (integer)
       rainbow => bt
       bt % ival = a1

    type is (logical)
       rainbow => d2t
       d2t % ival = a1
       d2t % lval = a2

    type is (character(*))
       if (len(a2) < 3) then
          rainbow => dt
          dt % ival = a1
          dt % cval = a2
       else
          rainbow => d3t
          d3t % ival = a1
          d3t % c3val = a2
       end if

    class default
       rainbow => null()
       print *, "Wrong argument for a2"

    end select

  end function rainbow

end program acetdt38
