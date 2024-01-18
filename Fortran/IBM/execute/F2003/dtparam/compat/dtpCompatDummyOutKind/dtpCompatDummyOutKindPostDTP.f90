!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutKind
!*  TEST CASE FILE             : dtpCompatDummyOutKindPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with KIND type parameter (success)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyOutKindLen (<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutKindPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutKindPostDTPmod
  use :: dtpCompatDummyOutKindPreDTPmod
  type, extends(base) :: child (k)
     integer, kind :: k
     character(3) :: chval
     integer(k) :: ival(3)
     integer(k) :: ival2
     character(3) :: chval2
  end type child

  interface
     subroutine dtpCompatDummyOutKindCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutKindCopy
  end interface

end module dtpCompatDummyOutKindPostDTPmod

program dtpCompatDummyOutKindPostDTP
  use :: dtpCompatDummyOutKindPostDTPmod
  class(base), allocatable, target :: b, c, b2, c2

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyOutKindCopy(b2,b)
  call test("b2", b2)

  print *, "allocating child"
  allocate(c, source=child(1)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "calling old implementation to copy; expecting no runtime error message"
  call dtpCompatDummyOutKindCopy(c2,c)
  call test("c2", c2)
  print *, "End."

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(1)); print *, txt, " - child(1):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(2)); print *, txt, " - child(2):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (child(4)); print *, txt, " - child(4):", a%i, ">", a%chval, "<", a%ival, a%ival2, ">", a%chval2, "<"
    type is (base);     print *, txt, " - base: ", a%i
    class default;      print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyOutKindPostDTP
