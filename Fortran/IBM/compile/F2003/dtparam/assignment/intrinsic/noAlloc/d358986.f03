!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assigning values to component of array of derived type
!*
!*  REFERENCE                  : Feature Number 358785
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : module, save
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Missing diagnostic test discovered in developing
!*  F2003/dtparam/assignment/intrinsic/noAlloc/dtpIAssignArray.f:
!*
!*  We shouldn't be able to apply an index to the middle name in a reference to
!*  an array of derived types which contain derived types.  Here we test this for
!*  array sections.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d358986
  type :: derived
     integer :: ifld
  end type derived

  type :: d2
     type(derived) :: der
  end type d2

  type(d2)  :: d(1)

  d(1)%der%ifld = 0 ! okay
  d(:)%der%ifld = 0 ! okay
  d%der%ifld = 0    ! okay

  d%der(1:1)%ifld = 0 ! using array section on wrong component
  d%der(:)%ifld = 0   ! ditto

end program d358986
