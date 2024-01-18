!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyInKindLen
!*  TEST CASE FILE             : dtpCompatDummyInKindLenPreDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code passes reference to extended type with both KIND and LEN type parameters in to subroutine
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatReturnValueKindLen (<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  Here we test an "old" module subroutine which copies an object of an extended class;
!*  the "post" code for this case uses both KIND and LEN type parameters.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyInKindLenPreDTPmod

  type base
     integer :: i
  end type base

  class(base), pointer :: p

contains

  subroutine dtpCompatDummyInKindLenCopy(arg)
    class(base), allocatable, intent(in) :: arg
    class(base), allocatable, target, save :: ba
    if (allocated(ba)) deallocate(ba)
    allocate(ba, source=arg)
    p => ba
  end subroutine dtpCompatDummyInKindLenCopy

end module dtpCompatDummyInKindLenPreDTPmod
