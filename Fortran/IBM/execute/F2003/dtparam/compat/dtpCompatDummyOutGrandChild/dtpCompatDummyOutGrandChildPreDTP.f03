!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE FILE             : dtpCompatDummyOutGrandChildPreDTP
!*
!*  DATE                       : 2009-06-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with KIND and LEN type parameters in grandchild / no DTP in child (fail)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpCompatDummyInGrandChild (<-dtpCompatDummyInKindLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  Here we test an "old" external subroutine which copies an object of an extended class;
!*  the "post" code for this case extends the base class with a non-DTP child, and then
!*  extends the child with a grandchild class which uses both KIND and LEN type parameters.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutGrandChildPreDTPmod

  type base
     integer :: i
  end type base

end module dtpCompatDummyOutGrandChildPreDTPmod

subroutine dtpCompatDummyOutGrandChildCopy(a1,a2)
  use :: dtpCompatDummyOutGrandChildPreDTPmod
  class(base), allocatable, intent(out) :: a1
  class(base), intent(in) :: a2
  allocate(a1, source=a2)
end subroutine dtpCompatDummyOutGrandChildCopy