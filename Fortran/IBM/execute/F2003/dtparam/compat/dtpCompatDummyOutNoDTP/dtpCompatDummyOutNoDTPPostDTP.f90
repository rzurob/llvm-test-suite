!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutNoDTP
!*  TEST CASE FILE             : dtpCompatDummyOutNoDTPPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with no type parameters (success)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyInNoDTP (<-dtpCompatDummyInKind<-dtpCompatDummyOutNoDTPLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutNoDTPPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutNoDTPPostDTPmod
  use :: dtpCompatDummyOutNoDTPPreDTPmod
  type, extends(base) :: child
     character(3) :: chval
     integer(1) :: ival(3)
     integer(1) :: ival2
     character(3) :: chval2
  end type child

  interface
     subroutine dtpCompatDummyOutNoDTPCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutNoDTPCopy
  end interface

end module dtpCompatDummyOutNoDTPPostDTPmod

program dtpCompatDummyOutNoDTPPostDTP
  use :: dtpCompatDummyOutNoDTPPostDTPmod
  class(base), allocatable, target :: b, c, d, e, b2, c2, e2

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyOutNoDTPCopy(b2,b)
  call test("b2", b2)

  print *, "allocating child"
  allocate(c, source=child(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "local copy; no failure expected"
  allocate(d, source=c)
  call test("d", d)

  print *, "calling old implementation to copy; not expecting runtime error message"
  call dtpCompatDummyOutNoDTPCopy(c2,c)
  print *, "old implementation successfully copied."
  call test("c2", c2)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child); print *, txt, " - child:", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (base);  print *, txt, " - base: ", a%i
    class default;   print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyOutNoDTPPostDTP
