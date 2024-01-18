!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeTypeBoundFunction01
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
!*  Invoke type-bound functions in a PARAMETER context (constants, not type parameters.)
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

  type, extends(allbase) :: b
     integer :: dat
   contains
     procedure :: sum1 => sumb
  end type b

  type, extends(allbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
   contains
     procedure :: sum1 => sum_tk4l
     procedure :: sum2 => sum_tk2l
     generic :: sum => sum2
  end type tkl

  type, extends(allbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
   contains
     procedure :: sum1 => sum_tk4
     procedure :: sum2 => sum_tk2
     generic :: sum => sum2
  end type tk

  type, extends(allbase) :: tl(l)
     integer, len  :: l
     integer(2) :: ifld(l)
   contains
     procedure :: sum1 => sum_tl
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

  integer function sumb(this)
    class(b), intent(in) :: this
    sumb = 991 + this % dat
  end function sumb

  integer function sum_tk4l(this)
    class(tkl(4,*)), intent(in) :: this
    sum_tk4l = sum(this % ifld)
  end function sum_tk4l

  integer function sum_tl(this)
    class(tl(*)), intent(in) :: this
    sum_tl = sum(this % ifld)
  end function sum_tl

  integer function sum_tk4(this)
    class(tk(4)), intent(in) :: this
    sum_tk4 = this % ifld
  end function sum_tk4

  integer function sum_tk2l(this)
    class(tkl(2,*)), intent(in) :: this
    sum_tk2l = sum(this % ifld)
  end function sum_tk2l

  integer function sum_tk2(this)
    class(tk(2)), intent(in) :: this
    sum_tk2 = this % ifld
  end function sum_tk2

end module dtpUseInvokeTypeBoundFunction01Mod

program dtpUseInvokeTypeBoundFunction01

  ! the renamings below should have no impact on the access we want; the only ones we're worried about are the type names we use
  use :: dtpUseInvokeTypeBoundFunction01Mod, sum1 => sum_tk4l, sum2 => sum_tl, sum3 => sum_tk4, sum4 => sum_tk2l, sum5 => sum_tk2, sum6 => sum_allbase, absBase => allbase

  implicit none

  integer :: i

  type (tkl(4,5)) :: tkl_45_1, tkl_45_2
  type (tk(4))    :: tk_4_1
  type (tl(5))    :: tl_5_1
  type (tkl(2,3)) :: tkl_23_1
  type (tk(2))    :: tk_2_1
  type (tl(3))    :: tl_3_1

  integer :: tkl45sum, tk4sum, tl5sum, tkl23sum, tk2sum, tl3sum

  type(tkl(4,2)), parameter :: tkl42Const = tkl(4,2)([91,102])
  type(tk(4)),    parameter :: tk4sumConst = tk(4)(22)
  type(tkl(2,1)), parameter :: tkl21sumConst = tkl(2,1)([89])
  type(tk(2)),    parameter :: tk2sumConst = tk(2)(10003)
  type(tl(2)),    parameter :: tl2sumConst = tl(2)([889,110])

  integer :: tkl42ConstSum
  integer :: tk4ConstSum
  integer :: tkl21ConstSum
  integer :: tk2ConstSum
  integer :: tl2ConstSum

  tkl42ConstSum = tkl42Const % sum()
  tk4ConstSum = tk4sumConst % sum()
  tkl21ConstSum = tkl21sumConst % sum()
  tk2ConstSum = tk2sumConst % sum()
  tl2ConstSum = tl2sumConst % sum()

  print *, "start"

  print *, tkl42ConstSum, tk4ConstSum, tkl21ConstSum, tk2ConstSum, tl2ConstSum

  tkl_45_1 % ifld     = [(i, i=0,4)]
  tk_4_1 % ifld       = 100000010
  tl_5_1 % ifld       = [(i, i=22,26)]
  tkl_23_1 % ifld     = [10030, 10031, 10032]
  tk_2_1 % ifld       = 10040
  tl_3_1 % ifld       = [50, 51, 52]

  tkl45sum = tkl_45_1 % sum()
  tk4sum   = tk_4_1 % sum()
  tl5sum   = tl_5_1 % sum()
  tkl23sum = tkl_23_1 % sum()
  tk2sum   = tk_2_1 % sum()
  tl3sum   = tl_3_1 % sum()

  print *, tkl45sum, tk4sum, tl5sum, tkl23sum, tk2sum, tl3sum

  tkl_mod_45_1 % ifld = [(i, i=100,104)]
  tk_mod_4_1 % ifld   = 100000110
  tl_mod_5_1 % ifld   = [(i, i=120,124)]
  tkl_mod_23_1 % ifld = [10130, 10131, 10132]
  tk_mod_2_1 % ifld   = 10140
  tl_mod_3_1 % ifld   = [53, 54, 55]

  print *, tkl_mod_45_1 % sum(), tk_mod_4_1 % sum(), tl_mod_5_1 % sum(), tkl_mod_23_1 % sum(), tk_mod_2_1 % sum(), tl_mod_3_1 % sum()

  print *, "end"

end program dtpUseInvokeTypeBoundFunction01
