!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatDummyOutKindLenPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with both KIND and LEN type parameters (fail)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyInKindLen (<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatDummyOutKindLenPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutKindLenPostDTPmod
  use :: dtpCompatDummyOutKindLenPreDTPmod
  type, extends(base) :: child (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l) :: chval
     integer(k) :: ival(l)
     integer(k) :: ival2
     character(l) :: chval2
  end type child

  interface
     subroutine dtpCompatDummyOutKindLenCopy(a1,a2)
       import :: base
       class(base), allocatable, intent(out) :: a1
       class(base), intent(in) :: a2
     end subroutine dtpCompatDummyOutKindLenCopy
  end interface

end module dtpCompatDummyOutKindLenPostDTPmod

program dtpCompatDummyOutKindLenPostDTP
  use :: dtpCompatDummyOutKindLenPostDTPmod
  class(base), allocatable, target :: b, c, b2, c2

  print *, "start"
  allocate(b, source=base(100))
  call test("b", b)
  print *, "calling old implementation to copy object of type base; not expecting runtime error message"
  call dtpCompatDummyOutKindLenCopy(b2,b)
  call test("b2", b2)

  print *, "allocating child"
  allocate(c, source=child(1,3)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatDummyOutKindLenCopy(c2,c)
  print *, "End. If you can see this, then the test failed."
  call test("c2", c2)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(1,*)); print *, txt, " - child(1,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (child(2,*)); print *, txt, " - child(2,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (child(4,*)); print *, txt, " - child(4,*):", a%i, a%l, len(a%chval), ">", a%chval, "<", size(a%ival), a%ival, a%ival2, len(a%chval2), ">", a%chval2, "<"
    type is (base);       print *, txt, " - base: ", a%i
    class default;        print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatDummyOutKindLenPostDTP
