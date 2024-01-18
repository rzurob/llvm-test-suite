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


module dtpUseInvokeTypeBoundSubroutine02Mod

  implicit none

  type, abstract :: allbase
   contains
     procedure :: diff1 => diff_allbase
     generic :: diff => diff1
  end type allbase

  type, extends(allbase), abstract :: tklbase
   contains
     procedure :: diff1 => diff_tklbase
  end type tklbase

  type, extends(allbase), abstract :: tkbase
   contains
     procedure :: diff1 => diff_tkbase
  end type tkbase

  type, extends(allbase), abstract :: tlbase
   contains
     procedure :: diff1 => diff_tlbase
  end type tlbase

  type, extends(tklbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l)
   contains
     procedure :: diff1 => diff_tk4l
     procedure :: diff2 => diff_tk2l
     generic :: diff => diff2
  end type tkl

  type, extends(tkbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
   contains
     procedure :: diff1 => diff_tk4
     procedure :: diff2 => diff_tk2
     generic :: diff => diff2
  end type tk

  type, extends(tlbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l)
   contains
     procedure :: diff1 => diff_tl
  end type tl


  type (tkl(4,5)) :: tkl_mod_45_1
  type (tk(4))    :: tk_mod_4_1
  type (tl(5))    :: tl_mod_5_1

  type (tkl(2,3)) :: tkl_mod_23_1
  type (tk(2))    :: tk_mod_2_1
  type (tl(3))    :: tl_mod_3_1

contains

  subroutine diff_allbase(this,a1)
    class(allbase), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "allbase"
  end subroutine diff_allbase

  subroutine diff_tklbase(this,a1)
    class(tklbase), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "tklbase"
  end subroutine diff_tklbase

  subroutine diff_tlbase(this,a1)
    class(tlbase), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "tlbase"
  end subroutine diff_tlbase

  subroutine diff_tkbase(this,a1)
    class(tkbase), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "tkbase"
  end subroutine diff_tkbase

  subroutine diff_tk4l(this,a1)
    class(tkl(4,*)), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "this=tkl(4,*): k=", this % k, ", l=", this%l, size(this%ifld), this%ifld
    select type (a1)
    class is (tkl(4,*))
      print *, "a1=tkl(4,*): k=", a1 % k, ", l=", a1%l, size(a1%ifld), a1%ifld
      if (a1%l==this%l) then
         print *, "same type, kind and len; content is ", merge("same","different",all(a1%ifld==this%ifld))
      else
         print *, "same type and kind, but not len:", this%l, a1%l
      end if
    class default
      print *, "different type, can't compare"
    end select
  end subroutine diff_tk4l

  subroutine diff_tl(this,a1)
    class(tl(*)), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "this=tl(*): l=", this%l, size(this%ifld), this%ifld
    select type (a1)
    class is (tl(*))
      print *, "a1=tl(*): l=", a1%l, size(a1%ifld), a1%ifld
      if (a1%l==this%l) then
         print *, "same type and len; content is ", merge("same","different",all(a1%ifld==this%ifld))
      else
         print *, "same type, but not len:", this%l, a1%l
      end if
    class default
      print *, "different type, can't compare"
    end select
  end subroutine diff_tl

  subroutine diff_tk4(this,a1)
    class(tk(4)), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "this=tk(4): k=", this%k, kind(this%ifld), this%ifld
    select type (a1)
    class is (tk(4))
      print *, "a1=tk(4): k=", a1%k, kind(a1%ifld), a1%ifld
      print *, "same type; content is ", merge("same","different",a1%ifld==this%ifld)
    class default
      print *, "different type, can't compare"
    end select
  end subroutine diff_tk4

  subroutine diff_tk2l(this,a1)
    class(tkl(2,*)), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "this=tkl(2,*): k=", this%k, ", l=", this%l, size(this%ifld), this%ifld
    select type (a1)
    class is (tkl(2,*))
      print *, "a1=tkl(2,*): k=", a1 % k, ", l=", a1%l, size(a1%ifld), a1%ifld
      if (a1%l==this%l) then
         print *, "same type, kind and len; content is ", merge("same","different",all(a1%ifld==this%ifld))
      else
         print *, "same type and kind, but not len:", this%l, a1%l
      end if
    class default
      print *, "different type, can't compare"
    end select
  end subroutine diff_tk2l

  subroutine diff_tk2(this,a1)
    class(tk(2)), intent(in) :: this
    class(*), intent(in) :: a1
    print *, "this=tk(2): k=", this%k, kind(this%ifld), this%ifld
    select type (a1)
    class is (tk(2))
      print *, "a1=tk(2): k=", a1%k, kind(a1%ifld), a1%ifld
      print *, "same type; content is ", merge("same","different",a1%ifld==this%ifld)
    class default
      print *, "different type, can't compare"
    end select
  end subroutine diff_tk2

end module dtpUseInvokeTypeBoundSubroutine02Mod

program dtpUseInvokeTypeBoundSubroutine02

  use dtpUseInvokeTypeBoundSubroutine02Mod
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
  tkl_45_2 % ifld     = [1,2,2,3,4]
  tk_4_1 % ifld       = 100000010
  tl_5_1 % ifld       = [(i, i=20,24)]
  tkl_23_1 % ifld     = [10030, 10031, 10032]
  tk_2_1 % ifld       = 10040
  tl_3_1 % ifld       = [50, 51, 52]

  call tkl_45_1 % diff (tkl_45_1) ! same
  call tk_4_1 % diff (tk_4_1)     ! same
  call tl_5_1 % diff (tl_5_1)     ! same
  call tkl_23_1 % diff (tkl_23_1) ! same
  call tk_2_1 % diff (tk_2_1)     ! same
  call tl_3_1 % diff (tl_3_1)     ! same

  call tkl_45_1 % diff (tkl_45_2) ! TKL match, but not same
  call tk_4_1 % diff (tk_2_1)     ! T but not K
  call tl_5_1 % diff (tl_3_1)     ! T but not L
  call tkl_23_1 % diff (tkl_45_1) ! T but not KL
  call tk_2_1 % diff (tk_4_1)     ! T but not K
  call tl_3_1 % diff (tl_5_1)     ! T but not L

  tkl_mod_45_1 % ifld     = [(i, i=100,104)]
  tk_mod_4_1 % ifld       = 100000110
  tl_mod_5_1 % ifld       = [(i, i=120,124)]
  tkl_mod_23_1 % ifld     = [10130, 10131, 10132]
  tk_mod_2_1 % ifld       = 10140
  tl_mod_3_1 % ifld       = [53, 54, 55]

  call tkl_mod_45_1 % diff (tkl_mod_45_1) ! same
  call tk_mod_4_1 % diff (tk_4_1)     ! diff content
  call tl_mod_5_1 % diff (tl_mod_5_1) ! same
  call tkl_mod_23_1 % diff (tkl_23_1) ! diff content
  call tk_mod_2_1 % diff (tk_mod_2_1) ! same
  call tl_mod_3_1 % diff (tl_3_1)     ! diff content

  print *, "end"

end program dtpUseInvokeTypeBoundSubroutine02
