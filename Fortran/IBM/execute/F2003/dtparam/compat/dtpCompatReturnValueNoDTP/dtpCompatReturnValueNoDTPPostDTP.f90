!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatReturnValueNoDTP
!*  TEST CASE FILE             : dtpCompatReturnValueNoDTPPostDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : "DTP" code returns pointer to extended type with no type parameters
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompat001 ()
!*
!*  DESCRIPTION
!*
!*  See description in dtpCompatReturnValueNoDTPPreDTP.f
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueNoDTPPostDTPmod
  use :: dtpCompatReturnValueNoDTPPreDTPmod
  type, extends(base) :: child
     character(3) :: ch
  end type child
end module dtpCompatReturnValueNoDTPPostDTPmod

program dtpCompatReturnValueNoDTPPostDTP
  use :: dtpCompatReturnValueNoDTPPostDTPmod
  class(base), allocatable, target :: c, d
  class(base), pointer :: p, p2

  print *, "start"
  allocate(c, source=child(99,'xyz'))
  call test("c", c)
  print *, "storing and recalling via old implementation"
  p2 => c
  call setBP(p2)
  p => getBP()
  print *, "allocate in new via pointer from old"
  allocate(d, source=p)
  call test("d", d)
  print *, "calling old implementation to copy; not expecting runtime error message"
  call dtpCompatReturnValueNoDTPCopy
  print *, "old implementation successfully copied."
  call test("bp2", bp2)

contains

  subroutine test(txt,a)
    character(*) :: txt
    class(base) :: a
    select type (a)
    type is (child); print *, txt, " - child:", a%i, a%ch, "<"
    type is (base);  print *, txt, " - base: ", a%i
    class default;   print *, "Unknown type."
    end select
  end subroutine test

end program dtpCompatReturnValueNoDTPPostDTP
