!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyInGrandChild
!*  TEST CASE FILE             : dtpCompatDummyInGrandChildPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code passes reference to grandchild extended type with both KIND and LEN type parameters (child does not use DTP)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompatDummyInKindLen (<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyInGrandChildPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyInGrandChildPostDTPmod
  use :: dtpCompatDummyInGrandChildPreDTPmod
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
end module dtpCompatDummyInGrandChildPostDTPmod

program dtpCompatDummyInGrandChildPostDTP
  use :: dtpCompatDummyInGrandChildPostDTPmod
  class(base), allocatable, target :: b, b2, c, d

  print *, "start"

  ! try first with the non-DTP types: base and child:
  allocate(b, source=base(100))
  call test("b #1", b)
  allocate(b2, source=child(101,'x'))
  call test("b2 #1", b2)
  print *, "calling old implementation to copy b; not expecting runtime error message"
  call dtpCompatDummyInGrandChildCopy(b)
  call test("b #2", p)
  print *, "calling old implementation to copy b2; not expecting runtime error message"
  call dtpCompatDummyInGrandChildCopy(b2)
  call test("b2 #2", p)

  ! Now the DTP stuff
  allocate(c, source=grandchild(1,3)(99,"z","abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c #1", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatDummyInGrandChildCopy(c)
  print *, "End. If you can see this, then the test failed."
  call test("c #2", p)

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

end program dtpCompatDummyInGrandChildPostDTP
