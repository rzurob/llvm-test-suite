!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseDTIORead01
!*
!*  DATE                       : 2008-10-08
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : DTIO - read and write
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  user-defined input.
!*   Fixes by JX 20100106: 1.) DTIO is non-advancing IO, so using formats like
!        I9 requires at least 9 spaces in the input.  These kind of formats are
!        replaced by listed-directed format.  2.) type-bound DTIO for
!        subclasses of allbase ALWAYS invokes readThing4.  This will be wrong
!        for tkl(2,*).  Add readTK2, readTKL2 to the formatted read.  3.) the
!        4th read statement, the input data overflow for the 2-byte integers.
!        Corrected those.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseDTIORead01mod

  implicit none

  type, abstract :: allbase
   contains
     generic :: read(formatted) => readThing4
     procedure :: readThing4 => readAllbase
  end type allbase

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld = -1
   contains
     generic :: read(formatted) => readThing4, readThing2
     procedure :: readThing4 => readTK4
     procedure :: readThing2 => readTK2
  end type tk

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l) = -2
   contains
     generic :: read(formatted) => readThing4, readThing2
     procedure :: readThing4 => readTK4L
     procedure :: readThing2 => readTK2L
  end type tkl

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l) = -3
   contains
     procedure :: readThing4 => readTL
  end type tl

  type (tkl(4,5)), save :: tkl_mod_45_1
  type (tk(4)), save    :: tk_mod_4_1
  type (tl(5)), save    :: tl_mod_5_1

  type (tkl(2,3)), save :: tkl_mod_23_1
  type (tk(2)), save    :: tk_mod_2_1
  type (tl(3)), save    :: tl_mod_3_1

contains

  subroutine readAllbase (dtv, unit, iotype, v_list, iostat, iomsg)
    class(allbase), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    stop 10
  end subroutine readAllbase

  subroutine readTK4 (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tk(4)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    read(unit,*) dtv % ifld
  end subroutine readTK4

  subroutine readTK2 (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tk(2)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    read(unit,*) dtv % ifld
  end subroutine readTK2

  subroutine readTK4L (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tkl(4,*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character(255) :: fmt
    write(fmt,"('(',i1,'i9)')") dtv%l
    read(unit,*) dtv%ifld
  end subroutine readTK4L

  subroutine readTK2L (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tkl(2,*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character(255) :: fmt
    write(fmt,"('(',i1,'i6)')") dtv%l
    read(unit,*) dtv%ifld
  end subroutine readTK2L

  subroutine readTL (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tl(*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character(255) :: fmt
    write(fmt,"('(',i1,'i5)')") dtv%l
    read(unit,*) dtv%ifld
  end subroutine readTL

end module dtpUseDTIORead01mod


program dtpUseDTIORead01

  use dtpUseDTIORead01mod
  implicit none

  type (tk(4)), target      :: tk_4_1
  type (tk(2)), target      :: tk_2_1
  type (tkl(4,5)), target   :: tkl_45_1
  type (tkl(2,3)), target   :: tkl_23_1
  type (tl(5)), target      :: tl_5_1

  type (tk(4)), pointer    :: tk_4_2
  type (tk(2)), pointer    :: tk_2_2
  type (tkl(4,:)), pointer :: tkl_45_2
  type (tkl(2,:)), pointer :: tkl_23_2
  type (tl(5)), pointer    :: tl_5_2


  read *, tk_4_1;    print *, tk_4_1
  read *, tk_2_1;    print *, tk_2_1

  read *, tkl_45_1;  print *, tkl_45_1
  read *, tkl_23_1;  print *, tkl_23_1

  read *, tl_5_1;    print *, tl_5_1

  tk_4_2   => tk_4_1
  tk_2_2   => tk_2_1
  tkl_45_2 => tkl_45_1
  tkl_23_2 => tkl_23_1
  tl_5_2   => tl_5_1

  read *, tk_4_2;    print *, tk_4_2
  read *, tk_2_2;    print *, tk_2_2

  read *, tkl_45_2;  print *, tkl_45_2
  read *, tkl_23_2;  print *, tkl_23_2

  read *, tl_5_2;    print *, tl_5_2

  allocate(tkl(2,9):: tkl_23_2)
  read *, tkl_23_2
  print *, tkl_23_2

  print *, "end"

end program dtpUseDTIORead01
