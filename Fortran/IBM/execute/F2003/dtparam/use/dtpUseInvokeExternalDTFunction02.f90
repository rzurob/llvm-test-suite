!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeExternalDTFunction02
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
!*  Verify that the correct type is used in calling external functions, specifically
!*  in returning values.  Each function (extFunTK4L, extFunTK4, extFunTL, extFunTK2L
!*  and extFunTK2) returns an object of the obvious type.
!*  This case differs from dtpUseInvokeExternalDTFunction01 in that the main program
!*  does not USE the module directly, but via the internal subroutine intSub.
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


program dtpUseInvokeExternalDTFunction02

  implicit none

  call intSub

contains

  subroutine intSub
    use dtpUseModule
    type (tkl(4,5)) :: tkl_45_1, tkl_45_2
    type (tk(4))    :: tk_4_1
    type (tl(5))    :: tl_5_1
    type (tl(3))    :: tl_3_1
    type (tkl(2,3)) :: tkl_23_1
    type (tk(2))    :: tk_2_1

    interface
       function extFunTypeTK4L(a1, n)
         import :: tkl
         integer :: a1, n
         type(tkl(4,n)) :: extFunTypeTK4L
       end function extFunTypeTK4L
       function extFunTypeTK4(a1)
         import :: tk
         integer :: a1
         type(tk(4)) :: extFunTypeTK4
       end function extFunTypeTK4
       function extFunTypeTL(a1, n)
         import :: tl
         integer :: a1, n
         type(tl(n)) :: extFunTypeTL
       end function extFunTypeTL
       function extFunTypeTK2L(a1, n)
         use dtpUseModule
         integer :: a1, n
         type(tkl(2,n+1)) :: extFunTypeTK2L
       end function extFunTypeTK2L
       function extFunTypeTK2(a1)
         use dtpUseModule
         integer :: a1
         type(tk(2)) :: extFunTypeTK2
       end function extFunTypeTK2
       function extFunTypeTLb(a1,n)
         import :: tl
         integer :: a1, n
         type(tl(n-1)) :: extFunTypeTLb
       end function extFunTypeTLb
       function extFunTypeTK4Lb(a1, n)
         import :: tkl
         integer :: a1, n
         type(tkl(4,n+1)) :: extFunTypeTK4Lb
       end function extFunTypeTK4Lb
    end interface

    print *, "A: start"

    tkl_45_1 % ifld     = 20
    tk_4_1 % ifld       = 21
    tl_5_1 % ifld       = 22
    tkl_23_1 % ifld     = 23
    tk_2_1 % ifld       = 24
    tkl_45_2 % ifld     = 25
    tl_3_1 % ifld       = 26

    print *, "B: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "C: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "D: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "E: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "F: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld
    print *, "G: tkl(4,*) k=",tkl_45_2%k,kind(tkl_45_2%ifld),"l=",tkl_45_2%l,size(tkl_45_2%ifld),"data=",tkl_45_2%ifld
    print *, "H: tl, l=", size(tl_3_1%ifld),"data=",tl_3_1%ifld

    tkl_45_1 = extFunTypeTK4L(39, 5)
    tk_4_1   = extFunTypeTK4(38)
    tl_5_1   = extFunTypeTL(37, 5)
    tkl_23_1 = extFunTypeTK2L(36, 3)
    tk_2_1   = extFunTypeTK2(35)
    tkl_45_2 = extFunTypeTK4Lb(34,4)
    tl_3_1   = extFunTypeTLb(33,4)

    print *, "I: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "J: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "K: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "L: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "M: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld
    print *, "N: tkl(4,*) k=",tkl_45_2%k,kind(tkl_45_2%ifld),"l=",tkl_45_2%l,size(tkl_45_2%ifld),"data=",tkl_45_2%ifld
    print *, "O: tl, l=", size(tl_3_1%ifld),"data=",tl_3_1%ifld

    print *, "P: end"

  end subroutine intSub

end program dtpUseInvokeExternalDTFunction02


function extFunTypeTK4L(a1, n)
  use dtpUseModule
  integer :: a1
  integer :: n
  type(tkl(4,n)) :: extFunTypeTK4L
  extFunTypeTK4L % ifld = a1
end function extFunTypeTK4L


function extFunTypeTK4(a1)
  use dtpUseModule
  integer :: a1
  type(tk(4)) :: extFunTypeTK4
  extFunTypeTK4 % ifld = a1
end function extFunTypeTK4


function extFunTypeTL(a1, n)
  use dtpUseModule
  integer :: a1
  integer :: n
  type(tl(n)) :: extFunTypeTL
  extFunTypeTL % ifld = a1
end function extFunTypeTL


function extFunTypeTK2L(a1, n)
  use dtpUseModule
  integer :: a1
  integer :: n
  type(tkl(2,n+1)) :: extFunTypeTK2L
  extFunTypeTK2L % ifld = (a1/3) ** 3
end function extFunTypeTK2L


function extFunTypeTK2(a1)
  use dtpUseModule
  integer :: a1
  type(tk(2)) :: extFunTypeTK2
  extFunTypeTK2 % ifld = a1 ** 2
end function extFunTypeTK2


function extFunTypeTLb(a1,n)
  use dtpUseModule
  integer :: a1, n
  integer :: i
  type(tl(n-1)) :: extFunTypeTLb
  do i = 1,n-1
     extFunTypeTLb % ifld(i) = mod(a1 + i, 123)
  end do
end function extFunTypeTLb


function extFunTypeTK4Lb(a1, n)
  use dtpUseModule
  integer :: a1, n
  integer :: i
  type(tkl(4,n+1)) :: extFunTypeTK4Lb
  do i = 1,n+1
     extFunTypeTK4Lb % ifld(i) = 123 * a1 - i
  end do
end function extFunTypeTK4Lb
