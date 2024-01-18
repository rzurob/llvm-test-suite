!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatReturnValueKindLen
!*  TEST CASE FILE             : dtpCompatReturnValueKindLenPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code returns pointer to extended type with both KIND and LEN type parameters
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatReturnValueKind (<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatReturnValueKindLenPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueKindLenPostDTPmod
  use :: dtpCompatReturnValueKindLenPreDTPmod
  type, extends(base) :: child (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l) :: chval
     integer(k) :: ival(l)
     integer(k) :: ival2
     character(l) :: chval2
  end type child
end module dtpCompatReturnValueKindLenPostDTPmod

program dtpCompatReturnValueKindLenPostDTP
  use :: dtpCompatReturnValueKindLenPostDTPmod
  class(base), allocatable, target :: c, d
  class(base), pointer :: p, p2

  print *, "start"
  allocate(c, source=child(1,3)(99,"abc",[123_1,45_1,67_1],89_1,"def"))
  call test("c", c)
  print *, "storing and recalling via old implementation"
  p2 => c
  call setBP(p2)
  p => getBP()
  print *, "allocate in new via pointer from old"
  allocate(d, source=p)
  call test("d", d)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatReturnValueKindLenCopy
  print *, "End. If you can see this, then the test failed."
  call test("bp2", bp2)

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

end program dtpCompatReturnValueKindLenPostDTP
