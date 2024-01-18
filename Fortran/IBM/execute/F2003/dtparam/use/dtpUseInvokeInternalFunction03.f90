!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeInternalFunction03
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
!*  KEYWORD(S)                 : internal procedure
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that internal functions can be invoked with DTP types and return the
!*  correct value.
!*  This test case differs from dtpUseInvokeInternalFunction02 in that objects
!*  are returned from the functions, which take integer arguments, rather than
!*  objects in and integers out.
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

end module dtpUseModule


module other
  use :: dtpUseModule, only: tickle => tkl, tick => tk, tail => tl
  type (tickle(2,3)), save :: tkl_other_23_1
  type (tick(2)), save    :: tk_other_2_1
  type (tail(3)), save    :: tl_other_3_1
end module other

program dtpUseInvokeInternalFunction03

  implicit none

  call intSub

contains

  subroutine intSub
    use :: dtpUseModule, only: tickle => tkl, tick => tk, tail => tl
    implicit none

    type (tickle(4,5)) :: tkl_45_1, tkl_45_2
    type (tick(4))     :: tk_4_1
    type (tail(5))     :: tl_5_1
    type (tickle(2,3)) :: tkl_23_1
    type (tick(2))     :: tk_2_1

    print *, "A: start"

    tkl_45_1 = intFunTypeTK4L(20,5)
    tk_4_1   = intFunTypeTK4(21)
    tl_5_1   = intFunTypeTL(22,5)
    tkl_23_1 = intFunTypeTK2L(23,3)
    tk_2_1   = intFunTypeTK2(24)

    print *, "B: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "C: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "D: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "E: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "F: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld

    call intSub2
    print *, "J: end"

  end subroutine intSub


  subroutine intSub2
    use :: other, only: tkl_23_1 => tkl_other_23_1, tk_2_1 => tk_other_2_1, tl_3_1 => tl_other_3_1, &
         tkl => tickle, tk => tick, tl => tail
    implicit none

    tkl_23_1 = intFunTypeTK2L(25,3)
    tk_2_1 = intFunTypeTK2(26)
    tl_3_1 = intFunTypeTLb(27,3)

    print *, "G: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "H: tk(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld
    print *, "I: tl, l=", size(tl_3_1%ifld),"data=",tl_3_1%ifld

  end subroutine intSub2


  function intFunTypeTK4L(a1, a2)
    use dtpUseModule
    implicit none
    integer, intent(in) :: a1, a2
    integer :: i
    type(tkl(4,a2)) :: intFunTypeTK4L
    intFunTypeTK4L % ifld = (/(a1 + 123 + i, i=1,a2)/)
  end function intFunTypeTK4L


  function intFunTypeTK4(a1)
    use dtpUseModule
    implicit none
    integer :: a1
    type(tk(4)) :: intFunTypeTK4
    intFunTypeTK4 % ifld = a1 -123
  end function intFunTypeTK4


  function intFunTypeTL(a1,a2)
    use dtpUseModule
    implicit none
    integer, intent(in) :: a1, a2
    type(tl(a2)) :: intFunTypeTL
    intFunTypeTL % ifld = 123 - a1
  end function intFunTypeTL


  function intFunTypeTK2L(a1,a2)
    use dtpUseModule
    implicit none
    integer, intent(in) :: a1, a2
    integer :: i
    type(tkl(2,a2)) :: intFunTypeTK2L
    do i = 1, a2
       intFunTypeTK2L % ifld(i) = (a1-i) ** 2
    end do
  end function intFunTypeTK2L


  function intFunTypeTK2(a1)
    use dtpUseModule
    implicit none
    integer :: a1
    type(tk(2)) :: intFunTypeTK2
    intFunTypeTK2 % ifld = a1 ** 2
  end function intFunTypeTK2


  function intFunTypeTLb(a1,a2)
    use dtpUseModule
    implicit none
    integer :: a1, a2, i
    integer(1) :: arr(a2+1)
    type(tl(a2+1)) :: intFunTypeTLb
    arr = (/(2*i,i=1,a2)/)
    intFunTypeTLb % ifld = a1 + arr
  end function intFunTypeTLb


  function intFunTypeTK4Lb(a1,a2)
    use dtpUseModule
    implicit none
    integer :: a1, a2
    integer :: i
    type(tkl(4,a2-1)) :: intFunTypeTK4Lb
    do i = 1,a2-1
       intFunTypeTK4Lb % ifld (i) = a1 / i
    end do
  end function intFunTypeTK4Lb

end program dtpUseInvokeInternalFunction03
