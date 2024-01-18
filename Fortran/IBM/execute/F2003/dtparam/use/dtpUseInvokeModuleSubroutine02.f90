!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseInvokeModuleSubroutine02
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
!*  Verify that the correct type is used in calling module subroutines and
!*  functions, in accessing use-associated values and passing arguments.
!*  - modSub1 tests use association
!*  - modSub2 tests argument passing (TYPE), including all intents
!*  - modSub3 tests argument passing (CLASS), including all intents
!*  This differs from dtpUseInvokeModuleSubroutine01 in that the module is not
!*  USEd in the main program, but in an internal subroutine invoked by the main
!*  program.
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

  subroutine modSub1
    use dtpUseTypesModule

    type (tkl(4,5)) :: tkl_45_1
    type (tk(4))    :: tk_4_1
    type (tl(5))    :: tl_5_1

    print *, "A: in modSub1"

    tkl_45_1 % ifld     = 12
    tkl_mod_45_1 % ifld = 13
    tk_4_1 % ifld       = 14
    tk_mod_4_1 % ifld   = 15
    tl_5_1 % ifld       = 16
    tl_mod_5_1 % ifld   = 17

    print *, "B: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "C: tkl(4,*) k=",tkl_mod_45_1%k,kind(tkl_mod_45_1%ifld),"l=",tkl_mod_45_1%l,size(tkl_mod_45_1%ifld),"data=",tkl_mod_45_1%ifld
    print *, "D: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "E: tk(4) k=",tk_mod_4_1%k,kind(tk_mod_4_1%ifld),"data=",tk_mod_4_1%ifld
    print *, "F: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "G: tl, l=", size(tl_mod_5_1%ifld),"data=",tl_mod_5_1%ifld
    print *, "H: end modSub1"

  end subroutine modSub1


  subroutine modSub2(a1, a2, a3, a4, a5)
    use dtpUseTypesModule
    type (tkl(4,*))              :: a1
    type (tk(4)), intent(in)     :: a2
    type (tl(*)), intent(inout)  :: a3
    type (tkl(2,*)), intent(out) :: a4
    type (tkl(4,*)), intent(out) :: a5
    type (tl(5))                 :: localTL

    print *, "I: in modSub2"
    print *, "J: tkl(4,*) k=",a1%k,kind(a1%ifld),"l=",a1%l,size(a1%ifld),"data=",a1%ifld
    print *, "K: tk(4) k=",a2%k,kind(a2%ifld),"data=",a2%ifld
    print *, "L: tl, l=", size(a3%ifld),"data=",a3%ifld

    a1 % ifld = a1 % ifld + 4
    localTL % ifld = 30
    a3 = localTL      ! Length must agree with actual argument
    a4 = tkl(2,3)(31) ! Length must agree with actual argument
    ! a5 purposely not assigned to, to test intent(out)
    print *, "M: end modSub2"

  end subroutine modSub2


  subroutine modSub3(a1, a2, a3)
    use dtpUseTypesModule
    class (tkl(2,*)) :: a1
    class (tk(2))    :: a2
    class (tl(*))    :: a3

    print *, "S: in modSub3"
    print *, "T: tkl(2,*) k=",a1%k,kind(a1%ifld),"l=",a1%l,size(a1%ifld),"data=",a1%ifld
    print *, "U: tk(2) k=",a2%k,kind(a2%ifld),"data=",a2%ifld
    print *, "V: tl, l=", size(a3%ifld),"data=",a3%ifld
    print *, "W: end modSub3"

  end subroutine modSub3

end module dtpUseRoutinesModule


program dtpUseInvokeModuleSubroutine02

  implicit none

  call intSub

contains

  subroutine intSub
    use :: dtpUseTypesModule
    use :: dtpUseRoutinesModule
    implicit none
    type (tkl(4,5)) :: tkl_45_1, tkl_45_2
    type (tk(4))    :: tk_4_1
    type (tl(5))    :: tl_5_1
    type (tkl(2,3)) :: tkl_23_1
    type (tk(2))    :: tk_2_1

    tkl_45_1 % ifld     = 18
    tk_4_1 % ifld       = 19
    tl_5_1 % ifld       = 20
    tkl_45_2 % ifld     = 21
    tkl_23_1 % ifld     = 22
    tk_2_1 % ifld       = 23

    call modSub1
    call modSub2(tkl_45_1, tk_4_1, tl_5_1, tkl_23_1, tkl_45_2)
    print *, "N: tkl(4,*) k=",tkl_45_1%k,kind(tkl_45_1%ifld),"l=",tkl_45_1%l,size(tkl_45_1%ifld),"data=",tkl_45_1%ifld
    print *, "O: tk(4) k=",tk_4_1%k,kind(tk_4_1%ifld),"data=",tk_4_1%ifld
    print *, "P: tl, l=", size(tl_5_1%ifld),"data=",tl_5_1%ifld
    print *, "Q: tkl(2,*) k=",tkl_23_1%k,kind(tkl_23_1%ifld),"l=",tkl_23_1%l,size(tkl_23_1%ifld),"data=",tkl_23_1%ifld
    print *, "R: tkl(4,*) k=",tkl_45_2%k,kind(tkl_45_2%ifld),"l=",tkl_45_2%l,size(tkl_45_2%ifld),"data=",tkl_45_2%ifld
    call modSub3(tkl_23_1, tk_2_1, tl_5_1)
    print *, "X: end"

  end subroutine intSub

end program dtpUseInvokeModuleSubroutine02
