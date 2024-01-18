!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeExternalDTFunction03
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : external procedure invocation
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
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
!*  This case differs from dtpUseInvokeExternalDTFunction02 in that some symbols are
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


program dtpUseInvokeExternalDTFunction03

  implicit none

  call intSub

contains

  subroutine intSub
    use :: dtpUseModule, only: chuckle => tkl, chuck => tk, churl => tl
    type (chuckle(4,5)) :: chuckle_45_1, chuckle_45_2
    type (chuck(4))    :: chuck_4_1
    type (churl(5))    :: churl_5_1
    type (churl(3))    :: churl_3_1
    type (chuckle(2,3)) :: chuckle_23_1
    type (chuck(2))    :: chuck_2_1

    interface
       function extFunTypeTK4L(a1, n)
         import :: chuckle
         integer :: a1, n
         type(chuckle(4,n)) :: extFunTypeTK4L
       end function extFunTypeTK4L
       function extFunTypeTK4(a1)
         import :: chuck
         integer :: a1
         type(chuck(4)) :: extFunTypeTK4
       end function extFunTypeTK4
       function extFunTypeTL(a1, n)
         import :: churl
         integer :: a1, n
         type(churl(n)) :: extFunTypeTL
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
         import :: churl
         integer :: a1, n
         type(churl(n+1)) :: extFunTypeTLb
       end function extFunTypeTLb
       function extFunTypeTK4Lb(a1, n)
         import :: chuckle
         integer :: a1, n
         type(chuckle(4,n-1)) :: extFunTypeTK4Lb
       end function extFunTypeTK4Lb
    end interface

    print *, "A: start"

    chuckle_45_1 % ifld = 20
    chuck_4_1 % ifld    = 21
    churl_5_1 % ifld    = 22
    chuckle_23_1 % ifld = 23
    chuck_2_1 % ifld    = 24
    chuckle_45_2 % ifld = 25
    churl_3_1 % ifld    = 26

    print *, "B: chuckle(4,*) k=",chuckle_45_1%k,kind(chuckle_45_1%ifld),"l=",chuckle_45_1%l,size(chuckle_45_1%ifld),"data=",chuckle_45_1%ifld
    print *, "C: chuck(4) k=",chuck_4_1%k,kind(chuck_4_1%ifld),"data=",chuck_4_1%ifld
    print *, "D: churl, l=", size(churl_5_1%ifld),"data=",churl_5_1%ifld
    print *, "E: chuckle(2,*) k=",chuckle_23_1%k,kind(chuckle_23_1%ifld),"l=",chuckle_23_1%l,size(chuckle_23_1%ifld),"data=",chuckle_23_1%ifld
    print *, "F: chuck(2) k=",chuck_2_1%k,kind(chuck_2_1%ifld),"data=",chuck_2_1%ifld
    print *, "G: chuckle(4,*) k=",chuckle_45_2%k,kind(chuckle_45_2%ifld),"l=",chuckle_45_2%l,size(chuckle_45_2%ifld),"data=",chuckle_45_2%ifld
    print *, "H: churl, l=", size(churl_3_1%ifld),"data=",churl_3_1%ifld

    chuckle_45_1 = extFunTypeTK4L(39, 5)
    chuck_4_1    = extFunTypeTK4(38)
    churl_5_1    = extFunTypeTL(37, 5)
    chuckle_23_1 = extFunTypeTK2L(36, 3)
    chuck_2_1    = extFunTypeTK2(35)
    chuckle_45_2 = extFunTypeTK4L(34, 5)
    churl_3_1    = extFunTypeTL(33, 3)

    print *, "I: chuckle(4,*) k=",chuckle_45_1%k,kind(chuckle_45_1%ifld),"l=",chuckle_45_1%l,size(chuckle_45_1%ifld),"data=",chuckle_45_1%ifld
    print *, "J: chuck(4) k=",chuck_4_1%k,kind(chuck_4_1%ifld),"data=",chuck_4_1%ifld
    print *, "K: churl, l=", size(churl_5_1%ifld),"data=",churl_5_1%ifld
    print *, "L: chuckle(2,*) k=",chuckle_23_1%k,kind(chuckle_23_1%ifld),"l=",chuckle_23_1%l,size(chuckle_23_1%ifld),"data=",chuckle_23_1%ifld
    print *, "M: chuck(2) k=",chuck_2_1%k,kind(chuck_2_1%ifld),"data=",chuck_2_1%ifld
    print *, "N: chuckle(4,*) k=",chuckle_45_2%k,kind(chuckle_45_2%ifld),"l=",chuckle_45_2%l,size(chuckle_45_2%ifld),"data=",chuckle_45_2%ifld
    print *, "O: churl, l=", size(churl_3_1%ifld),"data=",churl_3_1%ifld

    print *, "P: end"

  end subroutine intSub
  
end program dtpUseInvokeExternalDTFunction03


function extFunTypeTK4L(a1, n)
  use :: dtpUseModule, only: tkl
  integer :: a1
  integer :: n
  type(tkl(4,n)) :: extFunTypeTK4L
  extFunTypeTK4L % ifld = a1
end function extFunTypeTK4L


function extFunTypeTK4(a1)
  use :: dtpUseModule, only: check => tk
  integer :: a1
  type(check(4)) :: extFunTypeTK4
  extFunTypeTK4 % ifld = a1
end function extFunTypeTK4


function extFunTypeTL(a1, n)
  use :: dtpUseModule, only: tl
  integer :: a1
  integer :: n
  type(tl(n)) :: extFunTypeTL
  extFunTypeTL % ifld = a1
end function extFunTypeTL


function extFunTypeTK2L(a1, n)
  use :: dtpUseModule, only: tickle => tkl
  integer :: a1
  integer :: n
  type(tickle(2,n+1)) :: extFunTypeTK2L
  extFunTypeTK2L % ifld = (a1/3) ** 3
end function extFunTypeTK2L


function extFunTypeTK2(a1)
  use :: dtpUseModule, tick => tk
  integer :: a1
  type(tick(2)) :: extFunTypeTK2
  extFunTypeTK2 % ifld = a1 ** 2
end function extFunTypeTK2


function extFunTypeTLb(a1,n)
  use :: dtpUseModule, only: tl
  integer :: a1, n
  integer :: i
  type(tl(n+1)) :: extFunTypeTLb
  do i = 1,n+1
     extFunTypeTLb % ifld(i) = mod(a1 + i, 123)
  end do
end function extFunTypeTLb


function extFunTypeTK4Lb(a1, n)
  use :: dtpUseModule, only: tickle => tkl
  integer :: a1, n
  integer :: i
  type(tickle(4,n-1)) :: extFunTypeTK4Lb
  do i = 1,n-1
     extFunTypeTK4Lb % ifld(i) = 123 * a1 - i
  end do
end function extFunTypeTK4Lb
