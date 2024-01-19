!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatDummyOutKindDefInitPreDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : use of default initialisation of DTP objects from within 12.1 to older versions is safe with KIND only
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyOutGrandChildDefInit (<-dtpCompatDummyOutGrandChild<-dtpCompatDummyInGrandChild<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  Here we test an "old" external subroutine with a single INTENT(OUT) argument which does
!*  nothing.  The effect is to apply the default initialisation to the object passed in.
!*  The "post" code for this case extends the base class with a DTP child with a KIND parameter.
!*  KIND type parameters do not cause problems, so this test merely verifies that an INTENT(OUT)
!*  argument can be safely used, as long as only KIND type parameters are involved.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutKindDefInitPreDTPmod

  type base
     integer :: i = -1
  end type base

end module dtpCompatDummyOutKindDefInitPreDTPmod

subroutine dtpCompatDummyOutKindDefInitClear(a1)
  use :: dtpCompatDummyOutKindDefInitPreDTPmod
  class(base), intent(out) :: a1
end subroutine dtpCompatDummyOutKindDefInitClear
