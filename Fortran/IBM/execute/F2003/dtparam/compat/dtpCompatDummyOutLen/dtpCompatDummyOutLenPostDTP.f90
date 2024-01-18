!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutLen
!*  TEST CASE FILE             : dtpCompatDummyOutLenPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with LEN type parameter (fail)
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
!*  See description in dtpCompatDummyOutLenPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutLenPostDTPmod
  use :: dtpCompatDummyOutLenPreDTPmod
  type, extends(base) :: child (l)
     integer, len  :: l
     character(l) :: chval
     integer(1) :: ival(l)
     integer(1) :: ival2
     character(l) :: chval2
  end type child

  interface
     subroutine dtpCompatDummyOutLenCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutLenCopy
  end interface

end module dtpCompatDummyOutLenPostDTPmod

program dtpCompatDummyOutLenPostDTP
  use :: dtpCompatDummyOutLenPostDTPmod
  class(base), allocatable, target :: b, c, b2, c2

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyOutLenCopy(b2,b)
  call test("b2", b2)

  print *, "allocating child"
  allocate(c, source=child(3)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatDummyOutLenCopy(c2,c)
  print *, "End. If you can see this, then the test failed."
  call test("c2", c2)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(*)); print *, txt, " - child(*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (base);     print *, txt, " - base: ", a%i
    class default;      print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyOutLenPostDTP
