!*******************************************************************************
!*  ============================================================================
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
!*  user-defined I/O
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseDTIOWrite01mod

  implicit none

  type, abstract :: allbase
   contains
     generic :: write(formatted) => writeThing4
     procedure :: writeThing4 => writeAllbase
  end type allbase

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld = -1
   contains
     generic :: write(formatted) => writeThing4, writeThing2
     procedure :: writeThing4 => writeTK4
     procedure :: writeThing2 => writeTK2
  end type tk

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l) = -2
   contains
     generic :: write(formatted) => writeThing4, writeThing2
     procedure :: writeThing4 => writeTK4L
     procedure :: writeThing2 => writeTK2L
  end type tkl

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l) = -3
   contains
     procedure :: writeThing4 => writeTL
  end type tl

  type (tkl(4,5)), save :: tkl_mod_45_1
  type (tk(4)), save    :: tk_mod_4_1
  type (tl(5)), save    :: tl_mod_5_1

  type (tkl(2,3)), save :: tkl_mod_23_1
  type (tk(2)), save    :: tk_mod_2_1
  type (tl(3)), save    :: tl_mod_3_1

contains

  subroutine writeAllbase (dtv, unit, iotype, v_list, iostat, iomsg)
    class(allbase), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('Allbase')")
  end subroutine writeAllbase

  subroutine writeTK4 (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tk(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('tkl(4):',i9)") dtv % ifld
  end subroutine writeTK4

  subroutine writeTK2 (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tk(2)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('tk(2):',i6)") dtv % ifld
  end subroutine writeTK2

  subroutine writeTK4L (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tkl(4,*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('tkl(4,l=',i1,'):',5i9)") dtv%l, dtv%ifld
  end subroutine writeTK4L

  subroutine writeTK2L (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tkl(2,*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('tkl(2,l=',i1,'):',10i6)") dtv%l, dtv%ifld
  end subroutine writeTK2L

  subroutine writeTL (dtv, unit, iotype, v_list, iostat, iomsg)
    class(tl(*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    write(unit,"('tl(l=',i1,'):',10i5)") dtv%l, dtv%ifld
  end subroutine writeTL

end module dtpUseDTIOWrite01mod


program dtpUseDTIOWrite01

  use dtpUseDTIOWrite01mod
  implicit none

  type (tk(4)), target      :: tk_4_1
  type (tk(2)), target      :: tk_2_1
  type (tkl(4,5)), target   :: tkl_45_1
  type (tkl(2,3)), target   :: tkl_23_1
  type (tl(5)), target      :: tl_5_1

  class (allbase), pointer  :: ab_2
  class (tk(4)), pointer    :: tk_4_2
  class (tk(2)), pointer    :: tk_2_2
  class (tkl(4,5)), pointer :: tkl_45_2
  class (tkl(2,3)), pointer :: tkl_23_2
  class (tl(5)), pointer    :: tl_5_2


  tk_4_1 % ifld   = 19
  tk_2_1 % ifld   = 23

  tkl_45_1 % ifld = [137429,137439,137449,137459,137469]
  tkl_23_1 % ifld = [8822,4997,4229]

  tl_5_1 % ifld   = [20,51,52,53,54]

  print *, tk_4_1
  print *, tk_2_1
  print *, tkl_45_1
  print *, tkl_23_1
  print *, tl_5_1

  tk_4_2   => tk_4_1
  tk_2_2   => tk_2_1
  tkl_45_2 => tkl_45_1
  tkl_23_2 => tkl_23_1
  tl_5_2   => tl_5_1

  print *, tk_4_2
  print *, tk_2_1
  print *, tkl_45_1
  print *, tkl_23_1
  print *, tl_5_1

  allocate(tkl(2,9):: ab_2)

  select type (ab_2)
    class is (tkl(2,*))
        print *, ab_2
  end select

  deallocate(ab_2)

  print *, "end"

end program dtpUseDTIOWrite01
