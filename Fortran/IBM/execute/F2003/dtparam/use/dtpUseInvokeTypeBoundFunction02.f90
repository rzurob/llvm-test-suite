!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeTypeBoundFunction02
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : module procedure
!*
!*  REFERENCE                  : Feature Number 355310
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
!*  Invoke type-bound functions which return derived type values.
!*  (Similar to dtpUseInvokeTypeBoundFunction01, but derived type return values
!*  instead of intrinsic types, and simple type-bound procedures as well as generics.)
!*
!  Bug fixes: 1.) pointer ap: ap%describe will ALWAYS invoke describe1 (this
!  will NOT work for tkl(2) type.
!             2.) in all clone() functions, replace allocatable, target :: tmp
!             with pointer :: tmp.  Local allocatables will be deallocated after
!             the function calls.
!
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseInvokeTypeBoundFunction02Mod

  implicit none

  type, abstract :: allbase
   contains
     procedure :: clone1 => clone_allbase
     generic :: clone => clone1
     procedure :: describe1 => describe_allbase
     procedure(describe_allbase), deferred :: describe2
     generic :: describe => describe1
  end type allbase

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
   contains
     procedure :: combine2 => combine_tk2l
     procedure :: combine4 => combine_tk4l
     procedure :: clone1 => clone_tk4l
     procedure :: clone2 => clone_tk2l
     generic :: clone => clone2
     procedure :: describe1 => describe_tk4l
     procedure :: describe2 => describe_tk2l
     generic :: describe => describe2
  end type tkl

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
   contains
     procedure :: combine2 => combine_tk2
     procedure :: combine4 => combine_tk4
     procedure :: clone1 => clone_tk4
     procedure :: clone2 => clone_tk2
     generic :: clone => clone2
     procedure :: describe1 => describe_tk4
     procedure :: describe2 => describe_tk2
     generic :: describe => describe2
  end type tk

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l)
   contains
     procedure :: clone1 => clone_tl
     procedure :: describe1 => describe_tl
     procedure :: describe2 => describe_tl
     procedure :: combine => combine_tl
  end type tl


  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

contains

  function clone_allbase(this)
    class(allbase), intent(in) :: this
    class(allbase), pointer :: clone_allbase
    clone_allbase => null()
  end function clone_allbase

  function clone_tk4l(this)
    class(tkl(4,*)), intent(in) :: this
    class(allbase), pointer :: clone_tk4l
    type(tkl(4,this%l)), pointer :: tmp
    allocate(tmp)
    tmp % ifld = this % ifld
    clone_tk4l => tmp
  end function clone_tk4l

  function clone_tl(this)
    class(tl(*)), intent(in) :: this
    class(allbase), pointer :: clone_tl
    type(tl(this%l)), pointer :: tmp
    allocate(tmp)
    tmp % ifld = this % ifld
    clone_tl => tmp
  end function clone_tl

  function clone_tk4(this)
    class(tk(4)), intent(in) :: this
    class(allbase), pointer :: clone_tk4
    type(tk(4)), pointer :: tmp
    allocate(tmp)
    tmp % ifld = this % ifld
    clone_tk4 => tmp
  end function clone_tk4

  function clone_tk2l(this)
    class(tkl(2,*)), intent(in) :: this
    class(allbase), pointer :: clone_tk2l
    type(tkl(2,this%l)), pointer :: tmp
    allocate(tmp)
    tmp % ifld = this % ifld
    clone_tk2l => tmp
  end function clone_tk2l

  function clone_tk2(this)
    class(tk(2)), intent(in) :: this
    class(allbase), pointer :: clone_tk2
    type(tk(2)), pointer :: tmp
    allocate(tmp)
    tmp % ifld = this % ifld
    clone_tk2 => tmp
  end function clone_tk2


  subroutine describe_allbase(this)
    class(allbase), intent(in) :: this
    print *, "impossible"
  end subroutine describe_allbase

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

end module dtpUseInvokeTypeBoundFunction02Mod

program dtpUseInvokeTypeBoundFunction02

  use dtpUseInvokeTypeBoundFunction02Mod
  implicit none

  integer :: i
  class(allbase), pointer :: ap

  type (tkl(4,5)) :: tkl_45_1, tkl_45_2, tkl_45_3
  type (tk(4))    :: tk_4_1, tk_4_2, tk_4_3
  type (tl(5))    :: tl_5_1, tl_5_2, tl_5_3
  type (tkl(2,3)) :: tkl_23_1, tkl_23_2, tkl_23_3
  type (tk(2))    :: tk_2_1, tk_2_2, tk_2_3
  type (tl(3))    :: tl_3_1, tl_3_2, tl_3_3

  print *, "start"
  print *

  tkl_45_1 % ifld = [(i, i=0,4)]
  tk_4_1 % ifld   = 100000010
  tl_5_1 % ifld   = [(i, i=20,24)]
  tkl_23_1 % ifld = [10030, 10031, 10032]
  tk_2_1 % ifld   = 10040
  tl_3_1 % ifld   = [50, 51, 52]

  call tkl_45_1 % describe
  call tk_4_1 % describe
  call tl_5_1 % describe
  call tkl_23_1 % describe
  call tk_2_1 % describe
  call tl_3_1 % describe
  print *

  tkl_45_2 % ifld = [1, 1, 2, 3, 5]
  tk_4_2 % ifld   = 100000012
  tl_5_2 % ifld   = [-8, -13, -21, -34, -55]
  tkl_23_2 % ifld = [89, 144, 233]
  tk_2_2 % ifld   = 10042
  tl_3_2 % ifld   = [-1, -1, -2]

  call tkl_45_2 % describe
  call tk_4_2 % describe
  call tl_5_2 % describe
  call tkl_23_2 % describe
  call tk_2_2 % describe
  call tl_3_2 % describe
  print *

  ap => tkl_45_1 % clone();     call ap % describe
  ap => tk_4_1 % clone();       call ap % describe
  ap => tl_5_1 % clone();       call ap % describe
  ap => tkl_23_1 % clone();     call ap % describe2
  ap => tk_2_1 % clone();       call ap % describe2
  ap => tl_3_1 % clone();       call ap % describe
  print *

  tkl_45_3 = tkl_45_1 % combine4(tkl_45_2)
  tk_4_3   = tk_4_1   % combine4(tk_4_2)
  tl_5_3   = tl_5_1   % combine(tl_5_2)
  tkl_23_3 = tkl_23_1 % combine2(tkl_23_2)
  tk_2_3   = tk_2_1   % combine2(tk_2_2)
  tl_3_3   = tl_3_1   % combine(tl_3_2)

  call tkl_45_3 % describe
  call tk_4_3 % describe
  call tl_5_3 % describe
  call tkl_23_3 % describe
  call tk_2_3 % describe
  call tl_3_3 % describe
  print *

  tkl_mod_45_1 % ifld = [(i, i=100,104)]
  tk_mod_4_1 % ifld   = 100000110
  tl_mod_5_1 % ifld   = [(i, i=120,124)]
  tkl_mod_23_1 % ifld = [10130, 10131, 10132]
  tk_mod_2_1 % ifld   = 10140
  tl_mod_3_1 % ifld   = [53, 54, 55]

  call tkl_mod_45_1 % describe
  call tk_mod_4_1 % describe
  call tl_mod_5_1 % describe
  call tkl_mod_23_1 % describe
  call tk_mod_2_1 % describe
  call tl_mod_3_1 % describe
  print *

  ap => tkl_mod_45_1 % clone(); call ap % describe
  ap => tk_mod_4_1 % clone();   call ap % describe
  ap => tl_mod_5_1 % clone();   call ap % describe
  ap => tkl_mod_23_1 % clone(); call ap % describe2
  ap => tk_mod_2_1 % clone();   call ap % describe2
  ap => tl_mod_3_1 % clone();   call ap % describe
  print *

  tkl_45_3 = tkl_mod_45_1 % combine4(tkl_45_2)
  tk_4_3   = tk_mod_4_1   % combine4(tk_4_2)
  tl_5_3   = tl_mod_5_1   % combine(tl_5_2)
  tkl_23_3 = tkl_mod_23_1 % combine2(tkl_23_2)
  tk_2_3   = tk_mod_2_1   % combine2(tk_2_2)
  tl_3_3   = tl_mod_3_1   % combine(tl_3_2)

  call tkl_45_3 % describe
  call tk_4_3 % describe
  call tl_5_3 % describe
  call tkl_23_3 % describe
  call tk_2_3 % describe
  call tl_3_3 % describe
  print *

  print *, "end"

end program dtpUseInvokeTypeBoundFunction02
