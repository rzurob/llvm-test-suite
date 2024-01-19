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
!*  Invoke type-bound functions with no additional arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseInvokeTypeBoundFunction01Mod

  implicit none

  type, abstract :: allbase
   contains
     procedure :: sum1 => sum_allbase
     generic :: sum => sum1
  end type allbase

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
   contains
     procedure :: sum1 => sum_tk4l
     procedure :: sum2 => sum_tk2l
     generic :: sum => sum2
     procedure :: fun => fun_tk4l
  end type tkl

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
   contains
     procedure :: sum1 => sum_tk4
     procedure :: sum2 => sum_tk2
     generic :: sum => sum2
     procedure :: fun1 => fun_tk1
     procedure :: fun2 => fun_tk2
     procedure :: fun4 => fun_tk4
     generic :: fun => fun1, fun2, fun4
  end type tk

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l)
   contains
     procedure :: sum1 => sum_tl
     procedure :: fun => fun_tl
  end type tl


  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

contains

  integer function sum_allbase(this)
    class(allbase), intent(in) :: this
    sum_allbase = 0
  end function sum_allbase

  integer function sum_tk4l(this)
    class(tkl(4,*)), intent(in) :: this
    sum_tk4l = sum(int(this % ifld, kind(sum_tk4l)))
  end function sum_tk4l

  integer function sum_tl(this)
    class(tl(*)), intent(in) :: this
    sum_tl = sum(int(this % ifld, kind(sum_tl)))
  end function sum_tl

  integer function sum_tk4(this)
    class(tk(4)), intent(in) :: this
    sum_tk4 = this % ifld
  end function sum_tk4

  integer function sum_tk2l(this)
    class(tkl(2,*)), intent(in) :: this
    sum_tk2l = sum(int(this % ifld, kind(sum_tk2l)))
  end function sum_tk2l

  integer function sum_tk2(this)
    class(tk(2)), intent(in) :: this
    sum_tk2 = this % ifld
  end function sum_tk2


  integer function fun_tk4l(this, a1)
    class(tkl(4,*)), intent(in) :: this
    integer, intent(in) :: a1
    fun_tk4l = a1 * this % l * kind(this % ifld)
  end function fun_tk4l

  integer function fun_tl(this,a1)
    class(tl(*)), intent(in) :: this
    integer, intent(in) :: a1
    fun_tl = a1 * this % l * kind(this % ifld)
  end function fun_tl

  integer function fun_tk1(this,a1)
    class(tk(1)), intent(in) :: this
    integer, intent(in) :: a1
    fun_tk1 = a1 * kind(this % ifld)
  end function fun_tk1

  integer function fun_tk2(this,a1)
    class(tk(2)), intent(in) :: this
    integer, intent(in) :: a1
    fun_tk2 = a1 * kind(this % ifld)
  end function fun_tk2

  integer function fun_tk4(this,a1)
    class(tk(4)), intent(in) :: this
    integer, intent(in) :: a1
    fun_tk4 = a1 * kind(this % ifld)
  end function fun_tk4

end module dtpUseInvokeTypeBoundFunction01Mod

program dtpUseInvokeTypeBoundFunction01

  use :: dtpUseInvokeTypeBoundFunction01Mod, zap => allbase, sum_allbase => sum_tk4l, sum_tk4l => sum_tl, sum_tl => sum_tk4, sum_tk4 => sum_tk2l, sum_tk2l => sum_tk2, sum_tk2 => sum_allbase

  implicit none

  integer :: i

  type (tkl(4,5)) :: tkl_45_1, tkl_45_2
  type (tk(4))    :: tk_4_1
  type (tl(5))    :: tl_5_1
  type (tkl(2,3)) :: tkl_23_1
  type (tk(2))    :: tk_2_1
  type (tl(3))    :: tl_3_1

  integer :: tkl45sum, tk4sum, tl5sum, tkl23sum, tk2sum, tl3sum

  print *, "start"

  tkl_45_1 % ifld     = [(i, i=0,4)]
  tk_4_1 % ifld       = 100000010
  tl_5_1 % ifld       = [(i, i=20,24)]
  tkl_23_1 % ifld     = [10030, 10031, 10032]
  tk_2_1 % ifld       = 10040
  tl_3_1 % ifld       = [50, 51, 52]

  tkl45sum = tkl_45_1 % sum() ! 10
  tk4sum   = tk_4_1 % sum()   ! 100000010
  tl5sum   = tl_5_1 % sum()   ! 110
  tkl23sum = tkl_23_1 % sum() ! 30093
  tk2sum   = tk_2_1 % sum()   ! 10040
  tl3sum   = tl_3_1 % sum()   ! 153

  print *, tkl45sum, tk4sum, tl5sum, tkl23sum, tk2sum, tl3sum
  !        10,    100000010,    110,    30093,  10040, 153
  print *, tkl_45_1 % fun(10), tk_4_1 % fun(5), tl_5_1 % fun(3), tk_2_1 % fun(2), tl_3_1 % fun(1)
  !        200              20            15            4             3
  tkl_mod_45_1 % ifld = [(i, i=100,104)]
  tk_mod_4_1 % ifld   = 100000110
  tl_mod_5_1 % ifld   = [(i, i=120,124)]
  tkl_mod_23_1 % ifld = [10130, 10131, 10132]
  tk_mod_2_1 % ifld   = 10140
  tl_mod_3_1 % ifld   = [53, 54, 55]

  print *, tkl_mod_45_1 % sum(), tk_mod_4_1 % sum(), tl_mod_5_1 % sum(), tkl_mod_23_1 % sum(), tk_mod_2_1 % sum(), tl_mod_3_1 % sum()
  !        510,    100000110,    610,    30393,  10140, 162
  print *, tkl_mod_45_1 % fun(10), tk_mod_4_1 % fun(5), tl_mod_5_1 % fun(3), tk_mod_2_1 % fun(2), tl_mod_3_1 % fun(1)

  print *, "end"

end program dtpUseInvokeTypeBoundFunction01
