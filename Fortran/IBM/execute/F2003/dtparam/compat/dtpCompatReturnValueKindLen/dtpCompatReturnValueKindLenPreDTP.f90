!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatReturnValueKindLen
!*  TEST CASE FILE             : dtpCompatReturnValueKindLenPreDTP
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
!*  Here we test "old" module subroutines which manipulate class extensions; the "post"
!*  code for this case uses both KIND and LEN type parameters.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueKindLenPreDTPmod

  type base
     integer :: i
  end type base

  class(base), pointer :: bp, bp2

contains

  subroutine setBP(ap)
    class(base), pointer :: ap
    bp => ap
  end subroutine setBP

  function getBP()
    class(base), pointer :: getBP
    getBP => bp
  end function getBP

  subroutine dtpCompatReturnValueKindLenCopy
    class(base), pointer :: localp
    class(base), allocatable :: ba
    localp => getBP()
    allocate(bp2, source=localp)
  end subroutine dtpCompatReturnValueKindLenCopy

end module dtpCompatReturnValueKindLenPreDTPmod
