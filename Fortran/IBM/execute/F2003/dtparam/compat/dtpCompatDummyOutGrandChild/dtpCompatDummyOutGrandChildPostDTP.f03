!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatDummyOutGrandChildPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with KIND and LEN type parameters in grandchild / no DTP in child (fail)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyInGrandChild (<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutGrandChildPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutGrandChildPostDTPmod
  use :: dtpCompatDummyOutGrandChildPreDTPmod
  type, extends(base) :: child
     character(1) :: ch
  end type child
  type, extends(child) :: grandchild (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l) :: chval
     integer(k) :: ival(l)
     integer(k) :: ival2
     character(l) :: chval2
  end type grandchild

  interface
     subroutine dtpCompatDummyOutGrandChildCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutGrandChildCopy
  end interface

end module dtpCompatDummyOutGrandChildPostDTPmod

program dtpCompatDummyOutGrandChildPostDTP
  use :: dtpCompatDummyOutGrandChildPostDTPmod
  class(base), allocatable, target :: b, c, d, e, b2, c2, e2

  print *, "start"

  ! try first with the non-DTP types: base and child:
  allocate(b, source=base(100))
  call test("b", b)
  allocate(e, source=child(101,'x'))
  call test("e", e)
  print *, "calling old implementation to copy b; not expecting runtime error message"
  call dtpCompatDummyOutGrandChildCopy(b2, b)
  call test("b2", b2)
  print *, "calling old implementation to copy e; not expecting runtime error message"
  call dtpCompatDummyOutGrandChildCopy(e2, e)
  call test("e2", e2)

  ! Now the DTP stuff
  allocate(c, source=grandchild(1,3)(99,"z","abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatDummyOutGrandChildCopy(c2, c)
  print *, "End. If you can see this, then the test failed."
  call test("c2", c2)

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

end program dtpCompatDummyOutGrandChildPostDTP