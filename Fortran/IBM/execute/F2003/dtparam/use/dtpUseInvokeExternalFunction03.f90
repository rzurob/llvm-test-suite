!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : external procedure invocation
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : external procedure
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that external functions can be invoked with DTP types and return the
!*  correct value.
!*  This case differs from dtpUseInvokeExternalFunction02 in that some symbols are
!*  ONLY USEd and possibly renamed.
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


program dtpUseInvokeExternalFunction03

  implicit none

  call intSub

contains

  subroutine intSub
    use :: dtpUseModule, tinker => tkl, tink => tk, tilk => tl
    implicit none
    type (tinker(4,5)) :: tkl_45_1, tkl_45_2
    type (tink(4))    :: tk_4_1
    type (tilk(5))    :: tl_5_1
    type (tinker(2,3)) :: tkl_23_1
    type (tink(2))    :: tk_2_1

    integer :: a1, a2, a3, a4, a5, a6, a7

    interface
       integer function extFunTypeTK4L(a1)
         import :: tinker
         type(tinker(4,*)) :: a1
       end function extFunTypeTK4L
       integer function extFunTypeTK4(a1)
         import :: tink
         type(tink(4)) :: a1
       end function extFunTypeTK4
       integer function extFunTypeTL(a1)
         import :: tilk
         type(tilk(*)) :: a1
       end function extFunTypeTL
       integer function extFunTypeTK2L(a1)
         use :: dtpUseModule
         type(tkl(2,*)) :: a1
       end function extFunTypeTK2L
       integer function extFunTypeTK2(a1)
         use :: dtpUseModule
         type(tk(2)) :: a1
       end function extFunTypeTK2
       integer function extFunTypeTLb(a1,n)
         import :: tilk
         integer :: n
         type(tilk(n+1)) :: a1
       end function extFunTypeTLb
       integer function extFunTypeTK4Lb(a1,n)
         import :: tinker
         integer :: n
         type(tinker(4,n-1)) :: a1
       end function extFunTypeTK4Lb
    end interface

    print *, "A: start"

    tkl_45_1 % ifld     = 20
    tk_4_1 % ifld       = 21
    tl_5_1 % ifld       = 22
    tkl_23_1 % ifld     = 23
    tk_2_1 % ifld       = 24

    print *, "B: tinker(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "C: tink(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "D: tilk, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "E: tinker(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "F: tink(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld

    a1 = extFunTypeTK4L(tkl_45_1)
    a2 = extFunTypeTK4(tk_4_1)
    a3 = extFunTypeTL(tl_5_1)
    a4 = extFunTypeTK2L(tkl_23_1)
    a5 = extFunTypeTK2(tk_2_1)
    a6 = extFunTypeTLb(tl_5_1,4)
    a7 = extFunTypeTK4Lb(tkl_45_1,6)

    print *, "G: tinker(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld, a1, a7
    print *, "H: tink(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld, a2
    print *, "I: tilk, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld, a3, a6
    print *, "J: tinker(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld, a4
    print *, "K: tink(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld, a5

    print *, "L: end"

  end subroutine intSub

end program dtpUseInvokeExternalFunction03


integer function extFunTypeTK4L(a1)
  use :: dtpUseModule, only: tkl
  type(tkl(4,*)) :: a1
  extFunTypeTK4L = sum(a1%ifld) + 123
end function extFunTypeTK4L


integer function extFunTypeTK4(a1)
  use :: dtpUseModule, tick => tk
  type(tick(4)) :: a1
  extFunTypeTK4 = a1%ifld -123
end function extFunTypeTK4


integer function extFunTypeTL(a1)
  use :: dtpUseModule, tl => tl_mod_5_1, till => tl
  type(till(*)) :: a1
  extFunTypeTL = sum(int(a1%ifld,1_4)) * 123
end function extFunTypeTL


integer function extFunTypeTK2L(a1)
  use :: dtpUseModule, only: tkl
  type(tkl(2,*)) :: a1
  extFunTypeTK2L = (sum(int(a1%ifld,1_4))/3) ** 3
end function extFunTypeTK2L


integer function extFunTypeTK2(a1)
  use :: dtpUseModule
  type(tk(2)) :: a1
  extFunTypeTK2 = a1%ifld ** 2
end function extFunTypeTK2


integer function extFunTypeTLb(a1,n)
  use :: dtpUseModule, tail => tl
  integer :: n
  type(tail(n+1)) :: a1
  extFunTypeTLb = sum(int(a1%ifld,1_4))
end function extFunTypeTLb


integer function extFunTypeTK4Lb(a1,n)
  use :: dtpUseModule, only: tkl
  integer :: i, n
  type(tkl(4,n-1)) :: a1
  extFunTypeTK4Lb = 123
  do i = 1,n-1
     extFunTypeTK4Lb = extFunTypeTK4Lb + a1%ifld(i)
  end do
end function extFunTypeTK4Lb
