!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpCompatDummyOutNoDTP
!*  TEST CASE FILE             : dtpCompatDummyOutNoDTPPreDTP
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-06-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Broken Compatability Warning
!*
!*  SECONDARY FUNCTIONS TESTED : DTP code expects "old" code to copy extended type with no type parameters (success)
!*
!*  REFERENCE                  : Feature Number 366440
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpCompatDummyInNoDTP (<-dtpCompatDummyInKind<-dtpCompatDummyOutNoDTPLen<-dtpCompatReturnValueKindLen<-dtpCompatReturnValueKind<-dtpCompatReturnValueNoDTP<-dtpCompat001)
!*
!*  DESCRIPTION
!*
!*  Here we test an "old" external subroutine which copies an object of an extended class;
!*  the "post" code for this case uses no type parameters.
!*
!*  [Backwards compatibility between code compiled with "new" DTP-capable compiler versions
!*  (13.1 and above) and code compiled with "old" non-DTP-capable versions with polymorphism
!*  (11.1 and 12.1) has had to be broken in a small way: old code which references new DTP
!*  objects through polymorphic pointers or allocatables may generate a run-time message
!*  if it attempts to copy or initialise the referent or a copy of the referent.]
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpCompatDummyOutNoDTPPreDTPmod

  type base
     integer :: i
  end type base

end module dtpCompatDummyOutNoDTPPreDTPmod

subroutine dtpCompatDummyOutNoDTPCopy(a1,a2)
  use :: dtpCompatDummyOutNoDTPPreDTPmod
  class(base), allocatable, intent(out) :: a1
  class(base), intent(in) :: a2
  allocate(a1, source=a2)
end subroutine dtpCompatDummyOutNoDTPCopy
