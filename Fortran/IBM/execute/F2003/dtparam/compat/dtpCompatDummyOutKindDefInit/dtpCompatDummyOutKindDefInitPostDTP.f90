!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutKindDefInit
!*  TEST CASE FILE             : dtpCompatDummyOutKindDefInitPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of default initialisation of DTP objects from within 12.1 to older versions is safe with KIND only
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompatDummyOutGrandChildDefInit (<-dtpCompatDummyOutGrandChild<-dtpCompatDummyInGrandChild<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutKindDefInitPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutKindDefInitPostDTPmod
  use :: dtpCompatDummyOutKindDefInitPreDTPmod
  type, extends(base) :: child(k)
     integer, kind :: k
     character(3) :: chval  = 'pqrstuvwxyz'
     integer(k) :: ival(3)  = -2
     integer(k) :: ival2    = -3
     character(3) :: chval2 = 'zyxwvutsrqp'
  end type child

  interface
     subroutine dtpCompatDummyOutKindDefInitClear(a1)
       import :: base
       class(base), intent(out) :: a1
     end subroutine dtpCompatDummyOutKindDefInitClear
  end interface

end module dtpCompatDummyOutKindDefInitPostDTPmod

program dtpCompatDummyOutKindDefInitPostDTP
  use :: dtpCompatDummyOutKindDefInitPostDTPmod
  class(base), allocatable :: b, b1, c, c2

  print *, "start"

  ! try first with the non-DTP base type:
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to clear b; not expecting runtime error message"
  call dtpCompatDummyOutKindDefInitClear(b)
  call test("b #2", b)

  ! Now the DTP stuff
  allocate(c, source=child(1)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local clear; no failure expected"
  allocate(c2, source=c)
  call localClear(c2)
  call test("c2", c2)
  print *, "calling old implementation to clear c; expecting no runtime error message"
  call dtpCompatDummyOutKindDefInitClear(c)
  call test("c #2", c)
  print *, "End."

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(1)); print *, txt, " - child(1):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(2)); print *, txt, " - child(2):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(4)); print *, txt, " - child(4):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (base);            print *, txt, " - base: ", a%i
    class default;             print *, "Unknown type."
    end select
  end subroutine test

  subroutine localClear(a1)
    class(base), intent(out) :: a1
  end subroutine localClear

end program dtpCompatDummyOutKindDefInitPostDTP
