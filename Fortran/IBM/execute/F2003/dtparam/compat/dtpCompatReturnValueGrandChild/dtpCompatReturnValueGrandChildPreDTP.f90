!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatReturnValueGrandChildPreDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code returns pointer to grandchild extended type with both KIND and LEN type parameters (child does not use DTP)
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
!*  Here we test "old" module subroutines which manipulate class extensions; the "post"
!*  code for this case extends first the base class without using DTP, but then both KIND
!*  and LEN type parameters in the "grandchild".
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatReturnValueGrandChildPreDTPmod

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

  subroutine dtpCompatReturnValueGrandChildCopy
    class(base), pointer :: localp
    class(base), allocatable :: ba
    localp => getBP()
    allocate(bp2, source=localp)
  end subroutine dtpCompatReturnValueGrandChildCopy

end module dtpCompatReturnValueGrandChildPreDTPmod
