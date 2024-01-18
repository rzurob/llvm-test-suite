!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutGrandChildDefInit
!*  TEST CASE FILE             : dtpCompatDummyOutGrandChildDefInitPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of default initialisation of DTP objects from within 12.1 is also broken
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyOutGrandChild (<-dtpCompatDummyInGrandChild<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutGrandChildDefInitPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutGrandChildDefInitPostDTPmod
  use :: dtpCompatDummyOutGrandChildDefInitPreDTPmod
  type, extends(base) :: child
     character(1) :: ch = 'o'
  end type child
  type, extends(child) :: grandchild (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l) :: chval  = 'pqrstuvwxyz'
     integer(k) :: ival(l)  = -2
     integer(k) :: ival2    = -3
     character(l) :: chval2 = 'zyxwvutsrqp'
  end type grandchild

  interface
     subroutine dtpCompatDummyOutGrandChildDefInitClear(a1)
       import :: base
       class(base), intent(out) :: a1
     end subroutine dtpCompatDummyOutGrandChildDefInitClear
  end interface

end module dtpCompatDummyOutGrandChildDefInitPostDTPmod

program dtpCompatDummyOutGrandChildDefInitPostDTP
  use :: dtpCompatDummyOutGrandChildDefInitPostDTPmod
  class(base), allocatable :: b, b1, g, g2
  class(child), allocatable :: c

  print *, "start"

  ! try first with the non-DTP types: base and child:
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to clear b; not expecting runtime error message"
  call dtpCompatDummyOutGrandChildDefInitClear(b)
  call test("b #2", b)

  allocate(b1, source=child(101,'x'))
  call test("b1", b1)
  print *, "calling old implementation to clear b1; not expecting runtime error message"
  call dtpCompatDummyOutGrandChildDefInitClear(b1)
  call test("b1 #2", b1)

  allocate(c, source=child(102,'y'))
  call test("c", c)
  print *, "calling old implementation to clear c; not expecting runtime error message"
  call dtpCompatDummyOutGrandChildDefInitClear(c)
  call test("c #2", c)

  ! Now the DTP stuff
  allocate(g, source=grandchild(1,3)(99,"z","abc",[123_1,45_1,67_1],89_1,"def"))
  call test("g", g)
  print *, "local clear; no failure expected"
  allocate(g2, source=g)
  call localClear(g2)
  call test("g2", g2)
  print *, "calling old implementation to clear g; expecting runtime error message"
  call dtpCompatDummyOutGrandChildDefInitClear(g)
  print *, "End. If you can see this, then the test failed."
  call test("g #2", g)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (grandchild(1,*)); print *, txt, " - grandchild(1,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (grandchild(2,*)); print *, txt, " - grandchild(2,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (grandchild(4,*)); print *, txt, " - grandchild(4,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (child);           print *, txt, " - child:", a%i, ">", a%ch, "<"
    type is (base);            print *, txt, " - base: ", a%i
    class default;             print *, "Unknown type."
    end select
  end subroutine test

  subroutine localClear(a1)
    class(base), intent(out) :: a1
  end subroutine localClear

end program dtpCompatDummyOutGrandChildDefInitPostDTP
