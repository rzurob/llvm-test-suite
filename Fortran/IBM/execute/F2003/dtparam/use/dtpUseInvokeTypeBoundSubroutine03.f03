!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : module procedure
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
!*  Invoke type-bound subroutines with additional arguments but no renaming.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseInvokeTypeBoundSubroutine03Mod

  implicit none

  type, abstract :: allbase
   contains
     procedure :: describe1 => describe_allbase
     generic :: describe => describe1
  end type allbase

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
   contains
     procedure :: describe1 => describe_tk4l
     procedure :: describe2 => describe_tk2l
     generic :: describe => describe2
  end type tkl

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
   contains
     procedure :: describe1 => describe_tk4
     procedure :: describe2 => describe_tk2
     generic :: describe => describe2
  end type tk

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l)
   contains
     procedure :: describe1 => describe_tl
  end type tl


  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

contains

  subroutine describe_allbase(this)
    class(allbase), intent(in) :: this
    print *, "allbase"
  end subroutine describe_allbase

  subroutine describe_tk4l(this)
    class(tkl(4,*)), intent(in) :: this
    print *, "tkl(4,*): k=", this % k, ", l=", this%l, size(this%ifld), this%ifld
  end subroutine describe_tk4l

  subroutine describe_tl(this)
    class(tl(*)), intent(in) :: this
    print *, "tl(*): l=", this%l, size(this%ifld), this%ifld
  end subroutine describe_tl

  subroutine describe_tk4(this)
    class(tk(4)), intent(in) :: this
    print *, "tk(4): k=", this%k, kind(this%ifld), this%ifld
  end subroutine describe_tk4

  subroutine describe_tk2l(this)
    class(tkl(2,*)), intent(in) :: this
    print *, "tkl(2,*): k=", this%k, ", l=", this%l, size(this%ifld), this%ifld
  end subroutine describe_tk2l

  subroutine describe_tk2(this)
    class(tk(2)), intent(in) :: this
    print *, "tk(2): k=", this%k, kind(this%ifld), this%ifld
  end subroutine describe_tk2

end module dtpUseInvokeTypeBoundSubroutine03Mod

module dtpUseInvokeTypeBoundSubroutine03ModB
  use :: dtpUseInvokeTypeBoundSubroutine03Mod, describe_tk2 => describe_tk4, describe_tk4 => describe_tk2l, describe_tk2l => describe_tk2
end module dtpUseInvokeTypeBoundSubroutine03ModB

program dtpUseInvokeTypeBoundSubroutine03

  use dtpUseInvokeTypeBoundSubroutine03ModB
  implicit none

  integer :: i

  type (tkl(4,5)) :: tkl_45_1, tkl_45_2
  type (tk(4))    :: tk_4_1
  type (tl(5))    :: tl_5_1
  type (tkl(2,3)) :: tkl_23_1
  type (tk(2))    :: tk_2_1
  type (tl(3))    :: tl_3_1

  print *, "start"

  tkl_45_1 % ifld     = [(i, i=0,4)]
  tk_4_1 % ifld       = 100000010
  tl_5_1 % ifld       = [(i, i=20,24)]
  tkl_23_1 % ifld     = [10030, 10031, 10032]
  tk_2_1 % ifld       = 10040
  tl_3_1 % ifld       = [50, 51, 52]

  call tkl_45_1 % describe
  call tk_4_1 % describe
  call tl_5_1 % describe
  call tkl_23_1 % describe
  call tk_2_1 % describe
  call tl_3_1 % describe

  tkl_mod_45_1 % ifld     = [(i, i=100,104)]
  tk_mod_4_1 % ifld       = 100000110
  tl_mod_5_1 % ifld       = [(i, i=120,124)]
  tkl_mod_23_1 % ifld     = [10130, 10131, 10132]
  tk_mod_2_1 % ifld       = 10140
  tl_mod_3_1 % ifld       = [53, 54, 55]

  call tkl_mod_45_1 % describe
  call tk_mod_4_1 % describe
  call tl_mod_5_1 % describe
  call tkl_mod_23_1 % describe
  call tk_mod_2_1 % describe
  call tl_mod_3_1 % describe

  print *, "end"

end program dtpUseInvokeTypeBoundSubroutine03
