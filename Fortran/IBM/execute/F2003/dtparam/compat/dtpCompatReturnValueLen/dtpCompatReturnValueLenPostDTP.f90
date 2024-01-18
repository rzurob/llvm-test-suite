!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatReturnValueLen
!*  TEST CASE FILE             : dtpCompatReturnValueLenPostDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code returns pointer to extended type with LEN type parameter
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
!*  See description in dtpCompatReturnValueLenPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueLenPostDTPmod
  use :: dtpCompatReturnValueLenPreDTPmod
  type, extends(base) :: child (l)
     integer, len :: l
     character(l) :: chval
     integer(4) :: ifill
     integer(1) :: ival(l)
     integer(4) :: ifence
  end type child
end module dtpCompatReturnValueLenPostDTPmod

program dtpCompatReturnValueLenPostDTP
  use :: dtpCompatReturnValueLenPostDTPmod
  class(base), allocatable, target :: c, d
  class(base), pointer :: p, p2

  print *, "start"
  allocate(c, source=child(3)(11111111,"abc",123456789,[123_1,65_1,-31_1],-87654321))
  call test("c", c)
  print *, "storing and recalling via old implementation"
  p2 => c
  call setBP(p2)
  p => getBP()
  print *, "allocate in new via pointer from old"
  allocate(d, source=p)
  call test("d", d)
  print *, "calling old implementation to copy; expecting runtime error message"
  call dtpCompatReturnValueLenCopy
  print *, "End. If you can see this, then the test failed."
  call test("bp2", bp2)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child(*)); print *, txt, " - child(*):", a%i, a%l, len(a%chval), ">", a%chval, "<", a%ifill, a%ival, a%ifence
    type is (base);     print *, txt, " - base: ", a%i
    class default;      print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatReturnValueLenPostDTP
