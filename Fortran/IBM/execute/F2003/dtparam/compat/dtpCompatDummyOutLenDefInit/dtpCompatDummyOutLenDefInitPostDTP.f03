!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatDummyOutLenDefInitPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of default initialisation of DTP objects from within 12.1 is broken (LEN)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyOutKindDefInit (<-dtpCompatDummyOutGrandChildDefInit<-dtpCompatDummyOutGrandChild<-dtpCompatDummyInGrandChild<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutLenDefInitPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutLenDefInitPostDTPmod
  use :: dtpCompatDummyOutLenDefInitPreDTPmod
  type, extends(base) :: child(l)
     integer, len :: l
     character(l) :: chval  = 'pqrstuvwxyz'
     integer(1) :: ival(l)  = -2
     integer(1) :: ival2    = -3
     character(l) :: chval2 = 'zyxwvutsrqp'
  end type child

  interface
     subroutine dtpCompatDummyOutLenDefInitClear(a1)
       import :: base
       class(base), intent(out) :: a1
     end subroutine dtpCompatDummyOutLenDefInitClear
  end interface

end module dtpCompatDummyOutLenDefInitPostDTPmod

program dtpCompatDummyOutLenDefInitPostDTP
  use :: dtpCompatDummyOutLenDefInitPostDTPmod
  class(base), allocatable :: b, b1, c, c2

  print *, "start"

  ! try first with the non-DTP base type:
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to clear b; not expecting runtime error message"
  call dtpCompatDummyOutLenDefInitClear(b)
  call test("b #2", b)

  ! Now the DTP stuff
  allocate(c, source=child(3)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local clear; no failure expected"
  allocate(c2, source=c)
  call localClear(c2)
  call test("c2", c2)
  print *, "calling old implementation to clear c; expecting runtime error message"
  call dtpCompatDummyOutLenDefInitClear(c)
  print *, "End. If you can see this, then the test failed."
  call test("c #2", c)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(*)); print *, txt, " - child(*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (base);            print *, txt, " - base: ", a%i
    class default;             print *, "Unknown type."
    end select
  end subroutine test

  subroutine localClear(a1)
    class(base), intent(out) :: a1
  end subroutine localClear

end program dtpCompatDummyOutLenDefInitPostDTP
