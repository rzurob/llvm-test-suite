!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt38k_lkkl
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-11-16)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC in I/O,
!*                               as output-item, using DTIO, incl. polymorphism
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt38mod

  implicit none

  type Base (kBase_1) ! kBase_1=4
     integer, kind :: kBase_1
     integer(kBase_1) :: ival
   contains
     procedure :: printItem => basePrint
     generic :: write(formatted) => printItem
  end type Base

  type, extends(Base) :: Derived (lDerived_1) ! lDerived_1=1
     integer, len :: lDerived_1
     character(lDerived_1) :: cval
   contains
     procedure :: printItem => derivedPrint
  end type Derived

  type, extends(Base) :: Derived2 (kDerived2_1) ! kDerived2_1=4
     integer, kind :: kDerived2_1
     logical(kDerived2_1) :: lval
   contains
     procedure :: printItem => derived2Print
  end type Derived2

  type, extends(Base) :: Derived3 (kDerived3_1,lDerived3_1) ! kDerived3_1,lDerived3_1=1,3
     integer, kind :: kDerived3_1
     integer, len :: lDerived3_1
     character(len=lDerived3_1,kind=kDerived3_1) :: c3val
  end type Derived3

contains

  subroutine basePrint(this,unit,iotype,vlist,iostat,iomsg)
    class(Base(4)), intent(in) :: this ! tcx: (4)
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Base[',i0,']')", iostat=iostat) this % ival
  end subroutine basePrint

  subroutine derivedPrint(this,unit,iotype,vlist,iostat,iomsg)
    class(Derived(4,*)), intent(in) :: this ! tcx: (4,*)
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Derived[',i0,',',a,']')", iostat=iostat) this % ival, this % cval
  end subroutine derivedPrint

  subroutine derived2Print(this,unit,iotype,vlist,iostat,iomsg)
    class(Derived2(4,4)), intent(in) :: this ! tcx: (4,4)
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit, "('Derived2[',i0,',',l1,']')", iostat=iostat) this % ival, this % lval
  end subroutine derived2Print

end module acetdt38mod


program acetdt38k_lkkl

  use acetdt38mod
  implicit none

  type (Base(4)), target       :: b, bt ! tcx: (4)
  type (Derived(4,1)), target    :: d, dt ! tcx: (4,1)
  type (Derived2(4,4)), target   :: d2, d2t ! tcx: (4,4)
  type (Derived3(4,1,3)), target   :: d3, d3t ! tcx: (4,1,3)

  class (Base(4)), pointer     :: bp ! tcx: (4)
  class (Derived(4,:)), pointer  :: dp ! tcx: (4,:)
  class (Derived2(4,4)), pointer :: d2p ! tcx: (4,4)
  class (Derived3(4,1,:)), pointer :: d3p ! tcx: (4,1,:)

  b  = base(4)(1) ! tcx: (4)
  d  = derived(4,1)(2,'a') ! tcx: (4,1)
  d2 = derived2(4,4)(3,.true.) ! tcx: (4,4)
  d3 = derived3(4,1,3)(4,'abc') ! tcx: (4,1,3)

  bt  = base(4)(-1) ! tcx: (4)
  dt  = derived(4,1)(-2,'X') ! tcx: (4,1)
  d2t = derived2(4,4)(-3,.FALSE.) ! tcx: (4,4)
  d3t = derived3(4,1,3)(-4,'XXX') ! tcx: (4,1,3)

  print *, 'L 1:', b
  print *, 'L 2:', d
  print *, 'L 3:', d2
  print *, 'L 4:', d3

  ! The easy stuff: print known types with and without type specifiers:
  print *, 'L 5:', [Base(4):: b, d % Base, d2 % Base, d3 % Base], & ! tcx: (4)
                   [b,  d % Base, d2 % Base, d3 % Base]
  print *, 'L 6:', [Derived(4,1)::  d,  derived(4,1)(101,'b'), derived(4,1)(base=b,cval='c')],          & ! tcx: (4,1) ! tcx: (4,1) ! tcx: (4,1)
                   [d,  derived(4,1)(102,'d'), derived(4,1)(base=b,cval='e')] ! tcx: (4,1) ! tcx: (4,1)
  print *, 'L 7:', [Derived2(4,4):: d2, derived2(4,4)(103,.false.), derived2(4,4)(base=b,lval=.true.)], & ! tcx: (4,4) ! tcx: (4,4) ! tcx: (4,4)
                   [d2, derived2(4,4)(104,.false.), derived2(4,4)(base=b,lval=.false.)] ! tcx: (4,4) ! tcx: (4,4)
  print *, 'L 8:', [Derived3(4,1,3):: d3, derived3(4,1,3)(105,'bcd'), derived3(4,1,3)(base=b,c3val='def')],    & ! tcx: (4,1,3) ! tcx: (4,1,3) ! tcx: (4,1,3)
                   [d3, derived3(4,1,3)(106,'efg'), derived3(4,1,3)(base=b,c3val='fgh')] ! tcx: (4,1,3) ! tcx: (4,1,3)

  ! Try polymorphic pointers referencing the most basic type they can:
  bp  => b
  dp  => d
  d2p => d2
  d3p => d3
  print *, 'L 9:', [Base(4):: bp, dp % Base, d2p % Base, d3p % Base] ! tcx: (4)
  print *, 'L10:', [Derived(4,1)::  dp] ! tcx: (4,1)
  print *, 'L11:', [Derived2(4,4):: d2p] ! tcx: (4,4)
  print *, 'L12:', [Derived3(4,1,3):: d3p] ! tcx: (4,1,3)

  ! Repeat with anything more derived:
  bp  => d
  print *, 'L13:', [bp]
  bp  => d2
  print *, 'L14:', [bp]
  bp  => d3
  print *, 'L15:', [bp]

  bp  => d
  print *, 'L16:', [Base(4):: bp] ! tcx: (4)
  bp  => d2
  print *, 'L17:', [Base(4):: bp] ! tcx: (4)
  bp  => d3
  print *, 'L18:', [Base(4):: bp] ! tcx: (4)

  ! These work because rainbow returns a reference to a Base DT which
  ! has user-defined DTIO, even though the reference is polymorphic.

  print *, 'L19:', rainbow(5,2), rainbow(6,'a'), rainbow(7,.true.), rainbow(8,'def')
  print *, 'L20:', [rainbow(13,2), rainbow(14,'a'), rainbow(15,.true.), rainbow(16,'def')]
  print *, 'L21:', [Base(4):: rainbow(9,2), rainbow(10,'a'), rainbow(11,.true.), rainbow(12,'def')] ! tcx: (4)
  print *, 'L22:', [rainbow(17,2)]
  print *, 'L23:', [rainbow(18,'a')]
  print *, 'L24:', [rainbow(19,.true.)]
  print *, 'L25:', [rainbow(20,'def')]

contains

  function rainbow(a1, a2)
    integer   :: a1
    class(*)  :: a2
    class(Base(4)), pointer :: rainbow ! tcx: (4)

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

end program acetdt38k_lkkl


! Extensions to introduce derived type parameters:
! type: Base - added parameters (kBase_1) to invoke with (4)/declare with (4) - 12 changes
! type: Derived - added parameters (lDerived_1) to invoke with (4,1)/declare with (4,*) - 11 changes
! type: Derived2 - added parameters (kDerived2_1) to invoke with (4,4)/declare with (4,4) - 11 changes
! type: Derived3 - added parameters (kDerived3_1,lDerived3_1) to invoke with (4,1,3)/declare with (4,1,*) - 10 changes
