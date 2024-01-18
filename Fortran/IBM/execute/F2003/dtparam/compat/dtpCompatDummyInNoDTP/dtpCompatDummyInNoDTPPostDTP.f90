!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyInNoDTP
!*  TEST CASE FILE             : dtpCompatDummyInNoDTPPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code calls subroutine, passing in reference to extended type with no type parameters
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyInKind (<-dtpCompatDummyInNoDTPLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyInNoDTPPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyInNoDTPPostDTPmod
  use :: dtpCompatDummyInNoDTPPreDTPmod
  type, extends(base) :: child
     character(3) :: chval
     integer(1) :: ival(3)
     integer(1) :: ival2
     character(3) :: chval2
  end type child
end module dtpCompatDummyInNoDTPPostDTPmod

program dtpCompatDummyInNoDTPPostDTP
  use :: dtpCompatDummyInNoDTPPostDTPmod
  class(base), allocatable, target :: b, c, d

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyInNoDTPCopy(b)
  call test("p #1", p)
  print *, "allocating child"
  allocate(c, source=child(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)
  print *, "calling old implementation to copy; not expecting runtime error message"
  call dtpCompatDummyInNoDTPCopy(c)
  print *, "old implementation successfully copied."
  call test("p #2", p)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child); print *, txt, " - child:", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (base);       print *, txt, " - base: ", a%i
    class default;        print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyInNoDTPPostDTP
