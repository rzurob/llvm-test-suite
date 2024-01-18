!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutChildBase
!*  TEST CASE FILE             : dtpCompatDummyOutChildBasePostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : Pass %base part of DTP object in to copy routine (expect success)
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
!*  See description in dtpCompatDummyOutChildBasePreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutChildBasePostDTPmod
  use :: dtpCompatDummyOutChildBasePreDTPmod
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

  type, extends(base) :: kchild (k)
     integer, kind :: k
     integer(k) :: ival
  end type kchild

  type, extends(child) :: lchild (l)
     integer, len  :: l
     character(l) :: chval
  end type lchild

  interface
     subroutine dtpCompatDummyOutChildBaseCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutChildBaseCopy
  end interface

end module dtpCompatDummyOutChildBasePostDTPmod

program dtpCompatDummyOutChildBasePostDTP
  use :: dtpCompatDummyOutChildBasePostDTPmod
  class(base), allocatable, target :: b, b2, c2, c2b, d2, e2, f2
  class(child), allocatable, target :: c, e
  class(kchild(4)), allocatable, target :: d
  class(grandchild(1,3)), allocatable, target :: f

  print *, "start"

  ! try first with the non-DTP types: base and child:
  allocate(b, source=base(100))
  call test("b", b)
  call dtpCompatDummyOutChildBaseCopy(b2, b)
  call test("b2", b2)

  allocate(c, source=child(101,'x'))
  call test("c", c)
  call dtpCompatDummyOutChildBaseCopy(c2, c)
  call test("c2", c2)
  call dtpCompatDummyOutChildBaseCopy(c2b, c%base)
  call test("c2", c2b)

  allocate(d, source=kchild(4)(102,12481632))
  call test("d", d)
  call dtpCompatDummyOutChildBaseCopy(d2, d%base)
  call test("d2", d2)

  allocate(e, source=lchild(5)(103,"x","abcde"))
  call test("e", e)
  call dtpCompatDummyOutChildBaseCopy(e2, e%base)
  call test("e2", e2)

  allocate(f, source=grandchild(1,3)(99,"z","abc",[123_1,45_1,67_1],89_1,"def"))
  call test("f", f)
  call test("f%child", f%child)
  call dtpCompatDummyOutChildBaseCopy(f2, f%child)
  call test("f2", f2)

  print *, "end"

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)

    type is (kchild(1)); print *, txt, " - kchild(1):", a%i, a%ival
    type is (kchild(2)); print *, txt, " - kchild(2):", a%i, a%ival
    type is (kchild(4)); print *, txt, " - kchild(4):", a%i, a%ival

    type is (lchild(*)); print *, txt, " - lchild(*):", a%i, a%l, len(a%chval), ">", a%chval, "<"

    type is (grandchild(1,*)); print *, txt, " - grandchild(1,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (grandchild(2,*)); print *, txt, " - grandchild(2,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (grandchild(4,*)); print *, txt, " - grandchild(4,*):", a%i, ">", a%ch, "<", a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"

    type is (child);           print *, txt, " - child:", a%i, ">", a%ch, "<"

    type is (base);            print *, txt, " - base: ", a%i

    class default;             print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyOutChildBasePostDTP
