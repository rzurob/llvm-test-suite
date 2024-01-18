!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeInternalFunction02
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : internal procedure invocation
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : internal procedure
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that internal functions can be invoked with DTP types and return the
!*  correct value.
!*  This test case differs from dtpUseInvokeInternalFunction01 in that the
!*  module is unknown to the main program, and is only USEd in an internal
!*  subroutine.
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


module other
  use :: dtpUseModule, only: tickle => tkl, tick => tk, tail => tl
  type (tickle(2,3)), save :: tkl_other_23_1
  type (tick(2)), save    :: tk_other_2_1
  type (tail(3)), save    :: tl_other_3_1
end module other

program dtpUseInvokeInternalFunction02

  implicit none

  call intSub
  
contains


  subroutine intSub

    use dtpUseModule
    implicit none

    type (tkl(4,5)) :: tkl_45_1, tkl_45_2
    type (tk(4))    :: tk_4_1
    type (tl(5))    :: tl_5_1
    type (tkl(2,3)) :: tkl_23_1
    type (tk(2))    :: tk_2_1

    integer :: a1, a2, a3, a4, a5, a6, a7

    print *, "A: start"

    tkl_45_1 % ifld     = 20
    tk_4_1 % ifld       = 21
    tl_5_1 % ifld       = 22
    tkl_23_1 % ifld     = 23
    tk_2_1 % ifld       = 24

    print *, "B: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "C: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "D: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "E: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "F: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld

    a1 = intFunTypeTK4L(tkl_45_1)
    a2 = intFunTypeTK4(tk_4_1)
    a3 = intFunTypeTL(tl_5_1)
    a4 = intFunTypeTK2L(tkl_23_1)
    a5 = intFunTypeTK2(tk_2_1)
    a6 = intFunTypeTLb(tl_5_1,4)
    a7 = intFunTypeTK4Lb(tkl_45_1,6)

    print *, "G: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld, a1, a7
    print *, "H: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld, a2
    print *, "I: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld, a3, a6
    print *, "J: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld, a4
    print *, "K: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld, a5

    call intSub2
    print *, "O: end"

  end subroutine intSub


  subroutine intSub2
    use :: other, only: tkl_23_1 => tkl_other_23_1, tk_2_1 => tk_other_2_1, tl_3_1 => tl_other_3_1, &
         tkl => tickle, tk => tick, tl => tail
    implicit none
    integer :: i4, i5, i6

    tkl_23_1 % ifld     = 25
    tk_2_1 % ifld       = 26
    tl_3_1 % ifld       = 27

    i4 = intFunTypeTK2L(tkl_23_1)
    i5 = intFunTypeTK2(tk_2_1)
    i6 = intFunTypeTLb(tl_3_1,2)

    print *, "L: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld, i4
    print *, "M: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld, i5
    print *, "N: tl, l=", size(tl_3_1%ifld),"data=",tl_3_1%ifld, i6

  end subroutine intSub2


  integer function intFunTypeTK4L(a1)
    use dtpUseModule
    implicit none
    type(tkl(4,*)) :: a1
    intFunTypeTK4L = sum(a1%ifld) + 123
  end function intFunTypeTK4L


  integer function intFunTypeTK4(a1)
    use dtpUseModule
    implicit none
    type(tk(4)) :: a1
    intFunTypeTK4 = a1%ifld -123
  end function intFunTypeTK4


  integer function intFunTypeTL(a1)
    use dtpUseModule
    implicit none
    type(tl(*)) :: a1
    intFunTypeTL = sum(int(a1%ifld,1_4)) * 123
  end function intFunTypeTL


  integer function intFunTypeTK2L(a1)
    use dtpUseModule
    implicit none
    type(tkl(2,*)) :: a1
    intFunTypeTK2L = (sum(int(a1%ifld,1_4))/3) ** 3
  end function intFunTypeTK2L


  integer function intFunTypeTK2(a1)
    use dtpUseModule
    implicit none
    type(tk(2)) :: a1
    intFunTypeTK2 = a1%ifld ** 2
  end function intFunTypeTK2


  integer function intFunTypeTLb(a1,n)
    use dtpUseModule
    implicit none
    integer :: n
    type(tl(n+1)) :: a1
    intFunTypeTLb =  sum(int(a1%ifld,1_4))
  end function intFunTypeTLb


  integer function intFunTypeTK4Lb(a1,n)
    use dtpUseModule
    implicit none
    integer :: i, n
    type(tkl(4,n-1)) :: a1
    intFunTypeTK4Lb = 123
    do i = 1,n-1
       intFunTypeTK4Lb = intFunTypeTK4Lb + a1%ifld(i)
    end do
  end function intFunTypeTK4Lb

end program dtpUseInvokeInternalFunction02
