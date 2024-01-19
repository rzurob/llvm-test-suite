!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : procedures in interfaces
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
!*  Invoke procedures specified through an interface.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseInvokeInterfaceFunction01Mod

  implicit none

  type :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
  end type tkl

  type :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
  end type tk

  type :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l)
  end type tl

  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

contains

  subroutine describe_tk4l(this)
    class(tkl(4,*)), intent(in) :: this
    print *, "tkl(4,", this%l, ")", this % ifld
  end subroutine describe_tk4l

  subroutine describe_tl(this)
    class(tl(*)), intent(in) :: this
    print *, "tl(",this%l,")", this % ifld
  end subroutine describe_tl

  subroutine describe_tk4(this)
    class(tk(4)), intent(in) :: this
    print *, "tk(4)", this % ifld
  end subroutine describe_tk4

  subroutine describe_tk2l(this)
    class(tkl(2,*)), intent(in) :: this
    print *, "tkl(2,",this%l,"):", this % ifld
  end subroutine describe_tk2l

  subroutine describe_tk2(this)
    class(tk(2)), intent(in) :: this
    print *, "tk(2):", this % ifld
  end subroutine describe_tk2


  function combine_tk4l(this, that)
    class(tkl(4,*)), intent(in) :: this, that
    type(tkl(4,this%l)) :: combine_tk4l
    combine_tk4l % ifld = this % ifld + that % ifld
  end function combine_tk4l

  function combine_tl(this,that)
    class(tl(*)), intent(in) :: this, that
    type(tl(this%l)) :: combine_tl
    combine_tl % ifld = this % ifld + that % ifld
  end function combine_tl

  function combine_tk4(this,that)
    class(tk(4)), intent(in) :: this, that
    type(tk(4)) :: combine_tk4
    combine_tk4 % ifld = this % ifld + that % ifld
  end function combine_tk4

  function combine_tk2l(this,that)
    class(tkl(2,*)), intent(in) :: this, that
    type(tkl(2,this%l)) :: combine_tk2l
    combine_tk2l % ifld = this % ifld + that % ifld
  end function combine_tk2l

  function combine_tk2(this,that)
    class(tk(2)), intent(in) :: this, that
    type(tk(2)) :: combine_tk2
    combine_tk2 % ifld = this % ifld + that % ifld
  end function combine_tk2

end module dtpUseInvokeInterfaceFunction01Mod

program dtpUseInvokeInterfaceFunction01

  use :: dtpUseInvokeInterfaceFunction01Mod, describe_tk2 => describe_tk4, describe_tk4 => describe_tk2, describe_other => describe_tl, combine_tk4l => describe_tk2l, describe_tk2l => combine_tk4l
  implicit none

  integer :: i

  type (tkl(4,5)) :: tkl_45_1, tkl_45_2, tkl_45_3
  type (tk(4))    :: tk_4_1, tk_4_2, tk_4_3
  type (tl(5))    :: tl_5_1, tl_5_2, tl_5_3
  type (tkl(2,3)) :: tkl_23_1, tkl_23_2, tkl_23_3
  type (tk(2))    :: tk_2_1, tk_2_2, tk_2_3
  type (tl(3))    :: tl_3_1, tl_3_2, tl_3_3

  interface combine
     module procedure combine_tk2l
     module procedure combine_tk4
     module procedure combine_tk2
     module procedure describe_tk2l
     module procedure combine_tl
  end interface
  interface describe
     module procedure describe_tk2
     module procedure combine_tk4l
     module procedure describe_tk4
     module procedure describe_tk4l
     module procedure describe_other
  end interface

  print *, "start"
  print *

  tkl_45_1 % ifld = [(i, i=0,4)]
  tk_4_1 % ifld   = 100000010
  tl_5_1 % ifld   = [(i, i=20,24)]
  tkl_23_1 % ifld = [10030, 10031, 10032]
  tk_2_1 % ifld   = 10040
  tl_3_1 % ifld   = [50, 51, 52]

  call describe(tkl_45_1)
  call describe(tk_4_1)
  call describe(tl_5_1)
  call describe(tkl_23_1)
  call describe(tk_2_1)
  call describe(tl_3_1)
  print *

  tkl_45_2 % ifld = [1, 1, 2, 3, 5]
  tk_4_2 % ifld   = 100000012
  tl_5_2 % ifld   = [-8, -13, -21, -34, -55]
  tkl_23_2 % ifld = [89, 144, 233]
  tk_2_2 % ifld   = 10042
  tl_3_2 % ifld   = [-1, -1, -2]

  call describe(tkl_45_2)
  call describe(tk_4_2)
  call describe(tl_5_2)
  call describe(tkl_23_2)
  call describe(tk_2_2)
  call describe(tl_3_2)
  print *

  tkl_45_3 = combine(tkl_45_1, tkl_45_2)
  tk_4_3   = combine(tk_4_1, tk_4_2)
  tl_5_3   = combine(tl_5_1, tl_5_2)
  tkl_23_3 = combine(tkl_23_1, tkl_23_2)
  tk_2_3   = combine(tk_2_1, tk_2_2)
  tl_3_3   = combine(tl_3_1, tl_3_2)

  call describe(tkl_45_3)
  call describe(tk_4_3)
  call describe(tl_5_3)
  call describe(tkl_23_3)
  call describe(tk_2_3)
  call describe(tl_3_3)
  print *

  tkl_mod_45_1 % ifld = [(i, i=100,104)]
  tk_mod_4_1 % ifld   = 100000110
  tl_mod_5_1 % ifld   = [(i, i=120,124)]
  tkl_mod_23_1 % ifld = [10130, 10131, 10132]
  tk_mod_2_1 % ifld   = 10140
  tl_mod_3_1 % ifld   = [53, 54, 55]

  call describe(tkl_mod_45_1)
  call describe(tk_mod_4_1)
  call describe(tl_mod_5_1)
  call describe(tkl_mod_23_1)
  call describe(tk_mod_2_1)
  call describe(tl_mod_3_1)
  print *

  tkl_45_3 = combine(tkl_mod_45_1, tkl_45_2)
  tk_4_3   = combine(tk_mod_4_1, tk_4_2)
  tl_5_3   = combine(tl_mod_5_1, tl_5_2)
  tkl_23_3 = combine(tkl_mod_23_1, tkl_23_2)
  tk_2_3   = combine(tk_mod_2_1, tk_2_2)
  tl_3_3   = combine(tl_mod_3_1, tl_3_2)

  call describe(tkl_45_3)
  call describe(tk_4_3)
  call describe(tl_5_3)
  call describe(tkl_23_3)
  call describe(tk_2_3)
  call describe(tl_3_3)
  print *

  print *, "end"

end program dtpUseInvokeInterfaceFunction01
