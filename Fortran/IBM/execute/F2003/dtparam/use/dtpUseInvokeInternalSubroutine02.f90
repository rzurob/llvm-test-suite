!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : internal procedure invocation
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
!*  Invoke internal subroutines.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseModule

  implicit none

  type, abstract :: allbase
  end type allbase

  type, extends(allbase), abstract :: tklbase
  end type tklbase

  type, extends(allbase), abstract :: tkbase
  end type tkbase

  type, extends(allbase), abstract :: tlbase
  end type tlbase

  type, extends(tklbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l) = -99
  end type tkl

  type, extends(tkbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
  end type tk

  type, extends(tlbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l) = -42
  end type tl

  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

  save :: tkl_mod_23_1, tkl_mod_45_1, tl_mod_3_1, tl_mod_5_1

end module dtpUseModule


module extra
  use :: dtpUseModule, only: tickle => tkl, tick => tk, tail => tl
  type (tickle(4,5)) :: tkl_extra_45_1
  type (tick(4))    :: tk_extra_4_1
  type (tail(5))    :: tl_extra_5_1
  save :: tkl_extra_45_1, tk_extra_4_1, tl_extra_5_1
end module extra

program dtpUseInvokeInternalSubroutine02

  call intSub1
  print *, "end"

contains

  subroutine intSub1
    use dtpUseModule

    type (tkl(4,5)) :: tkl_45_1
    type (tk(4))    :: tk_4_1
    type (tl(5))    :: tl_5_1

    print *, "in intSub1"

    tkl_45_1 % ifld     = 12
    tkl_mod_45_1 % ifld = 13
    tk_4_1 % ifld       = 14
    tk_mod_4_1 % ifld   = 15
    tl_5_1 % ifld       = 16
    tl_mod_5_1 % ifld   = 17

    print *, tkl_45_1, "l:", tkl_45_1 % l, "k:", tkl_45_1 % k, kind(tkl_45_1%ifld), size(tkl_45_1%ifld)
    print *, tkl_mod_45_1, "l:", tkl_mod_45_1 % l, "k:", tkl_mod_45_1 % k, kind(tkl_mod_45_1%ifld), size(tkl_mod_45_1%ifld)

    print *, tk_4_1, "k:", tk_4_1 % k, kind(tk_4_1%ifld)
    print *, tk_mod_4_1, "k:", tk_mod_4_1 % k, kind(tk_mod_4_1%ifld)

    print *, tl_5_1, "l:", tl_5_1 % l, "k:", kind(tl_5_1%ifld), size(tl_5_1%ifld)
    print *, tl_mod_5_1, "l:", tl_mod_5_1 % l, "k:", kind(tl_mod_5_1%ifld), size(tl_mod_5_1%ifld)

    call intSub2

    print *, "end intSub1"

  end subroutine intSub1


  subroutine intSub2
    use extra

    type (tickle(4,5)) :: tkl_45_1
    type (tick(4))    :: tk_4_1
    type (tail(5))    :: tl_5_1

    print *, "start intSub2"
    tkl_extra_45_1 % ifld = 17
    tk_extra_4_1 % ifld   = 18
    tl_extra_5_1 % ifld   = 19
    tkl_45_1 % ifld       = 20
    tk_4_1 % ifld         = 21
    tl_5_1 % ifld         = 22

    print *, "calling intSub3(tkl_extra_45_1, tk_extra_4_1, tl_extra_5_1)"
    call intSub3(tkl_extra_45_1, tk_extra_4_1, tl_extra_5_1)
    print *, tkl_extra_45_1, "l:", tkl_extra_45_1 % l, "k:", tkl_extra_45_1 % k, kind(tkl_extra_45_1%ifld), size(tkl_extra_45_1%ifld)
    print *, tk_extra_4_1, "k:", tk_extra_4_1 % k, kind(tk_extra_4_1%ifld)
    print *, tl_extra_5_1, "l:", tl_extra_5_1 % l, "k:", kind(tl_extra_5_1%ifld), size(tl_extra_5_1%ifld)

    print *, "calling intSub3(tkl_45_1, tk_4_1, tl_5_1)"
    call intSub3(tkl_45_1, tk_4_1, tl_5_1)
    print *, tkl_45_1, "l:", tkl_45_1 % l, "k:", tkl_45_1 % k, kind(tkl_45_1%ifld), size(tkl_45_1%ifld)
    print *, tk_4_1, "k:", tk_4_1 % k, kind(tk_4_1%ifld)
    print *, tl_5_1, "l:", tl_5_1 % l, "k:", kind(tl_5_1%ifld), size(tl_5_1%ifld)

    print *, "end intSub2"

  end subroutine intSub2


  subroutine intSub3(a1, a2, a3)
    use dtpUseModule
    type (tkl(4,*)) :: a1
    type (tk(4))    :: a2
    type (tl(*))    :: a3

    print *, "in intSub3"

    print *, a1, "l:", a1 % l, "k:", a1 % k, kind(a1%ifld), size(a1%ifld)
    print *, a2, "k:", a2 % k, kind(a2%ifld)
    print *, a3, "l:", a3 % l, "k:", kind(a3%ifld), size(a3%ifld)

    print *, "end intSub3"

  end subroutine intSub3

end program dtpUseInvokeInternalSubroutine02
