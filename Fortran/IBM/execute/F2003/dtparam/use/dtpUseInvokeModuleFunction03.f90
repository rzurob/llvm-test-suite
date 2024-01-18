!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeModuleFunction01
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
!*  Verify that module functions can be invoked with DTP types and return the
!*  correct value.
!*  This case differs from dtpUseInvokeModuleFunction02 in that some symbols are
!*  ONLY USEd and possibly renamed.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseTypesModule

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

end module dtpUseTypesModule


module dtpUseRoutinesModule
  implicit none
contains
  integer function modFunTypeTK4L(a1)
    use :: dtpUseTypesModule, only: tkl
    type(tkl(4,*)) :: a1
    modFunTypeTK4L = sum(a1%ifld) + 123
  end function modFunTypeTK4L


  integer function modFunTypeTK4(a1)
    use :: dtpUseTypesModule, tick => tk
    type(tick(4)) :: a1
    modFunTypeTK4 = a1%ifld -123
  end function modFunTypeTK4


  integer function modFunTypeTL(a1)
    use :: dtpUseTypesModule, tl => tl_mod_5_1, till => tl
    type(till(*)) :: a1
    modFunTypeTL = sum(int(a1%ifld,1_4)) * 123
  end function modFunTypeTL


  integer function modFunTypeTK2L(a1)
    use :: dtpUseTypesModule, only: tkl
    type(tkl(2,*)) :: a1
    modFunTypeTK2L = (sum(int(a1%ifld,1_4))/3) ** 3
  end function modFunTypeTK2L


  integer function modFunTypeTK2(a1)
    use :: dtpUseTypesModule
    type(tk(2)) :: a1
    modFunTypeTK2 = a1%ifld ** 2
  end function modFunTypeTK2


  integer function modFunTypeTLb(a1,n)
    use :: dtpUseTypesModule, tail => tl
    integer :: n
    type(tail(n+1)) :: a1
    modFunTypeTLb = sum(int(a1%ifld,1_4))
  end function modFunTypeTLb


  integer function modFunTypeTK4Lb(a1,n)
    use :: dtpUseTypesModule, only: tkl
    integer :: i, n
    type(tkl(4,n-1)) :: a1
    modFunTypeTK4Lb = 123
    do i = 1,n-1
       modFunTypeTK4Lb = modFunTypeTK4Lb + a1%ifld(i)
    end do
  end function modFunTypeTK4Lb

end module dtpUseRoutinesModule


program dtpUseInvokeModuleFunction03

  implicit none

  call intSub

contains

  subroutine intSub
    use :: dtpUseTypesModule, tinker => tkl, tink => tk, tilk => tl
    use :: dtpUseRoutinesModule
    implicit none
    type (tinker(4,5)) :: tkl_45_1, tkl_45_2
    type (tink(4))    :: tk_4_1
    type (tilk(5))    :: tl_5_1
    type (tinker(2,3)) :: tkl_23_1
    type (tink(2))    :: tk_2_1

    integer :: a1, a2, a3, a4, a5, a6, a7

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

    a1 = modFunTypeTK4L(tkl_45_1)
    a2 = modFunTypeTK4(tk_4_1)
    a3 = modFunTypeTL(tl_5_1)
    a4 = modFunTypeTK2L(tkl_23_1)
    a5 = modFunTypeTK2(tk_2_1)
    a6 = modFunTypeTLb(tl_5_1,4)
    a7 = modFunTypeTK4Lb(tkl_45_1,6)

    print *, "G: tinker(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld, a1, a7
    print *, "H: tink(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld, a2
    print *, "I: tilk, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld, a3, a6
    print *, "J: tinker(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld, a4
    print *, "K: tink(2) k=",tk_2_1%k,kind(tk_2_1%ifld),"data=",tk_2_1%ifld, a5

    print *, "L: end"

  end subroutine intSub
  
end program dtpUseInvokeModuleFunction03
