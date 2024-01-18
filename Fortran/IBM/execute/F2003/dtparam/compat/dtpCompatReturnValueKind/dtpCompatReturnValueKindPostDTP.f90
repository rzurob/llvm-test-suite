!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatReturnValueKind
!*  TEST CASE FILE             : dtpCompatReturnValueKindPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code returns pointer to extended type with KIND type parameter
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatReturnValueNoDTP (<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatReturnValueKindPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueKindPostDTPmod
  use :: dtpCompatReturnValueKindPreDTPmod
  type, extends(base) :: child (k)
     integer, kind :: k
     integer(k) :: ival
     integer(4) :: ifill
     integer(k) :: ival2
  end type child
end module dtpCompatReturnValueKindPostDTPmod

program dtpCompatReturnValueKindPostDTP
  use :: dtpCompatReturnValueKindPostDTPmod
  class(base), allocatable, target :: c, d
  class(base), pointer :: p, p2

  print *, "start"
  allocate(c, source=child(1)(99,123,123456789,-121))
  call test("c", c)
  print *, "storing and recalling via old implementation"
  p2 => c
  call setBP(p2)
  p => getBP()
  print *, "allocate in new via pointer from old"
  allocate(d, source=p)
  call test("d", d)
  print *, "calling old implementation to copy; expecting no runtime error message"
  call dtpCompatReturnValueKindCopy
  call test("bp2", bp2)
  print *, "End."

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(1)); print *, txt, " - child(1):", a%i, a%ival, a%ifill, a%ival2
    type is (child(2)); print *, txt, " - child(2):", a%i, a%ival, a%ifill, a%ival2
    type is (child(4)); print *, txt, " - child(4):", a%i, a%ival, a%ifill, a%ival2
    type is (base);     print *, txt, " - base: ", a%i
    class default;      print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatReturnValueKindPostDTP
