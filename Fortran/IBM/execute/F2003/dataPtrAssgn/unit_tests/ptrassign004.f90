!****************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890


   integer , pointer :: ptr(:)
   integer , target, allocatable :: tar(:)

   allocate(tar(10), source=(/(i*i,i=1,10)/))

   ptr(5:)=>tar

  if(lbound(ptr, dim=1).ne. 5) error stop 1
  if(ubound(ptr, dim=1).ne. 14) error stop 2
  if(any(shape(ptr).ne.(/10/))) error stop 3
  if(.not.associated(ptr,tar)) error stop 4

  deallocate(tar)

  if(lbound(ptr, dim=1).ne. 5) error stop 5
  if(ubound(ptr, dim=1).ne. 14) error stop 6
  if(any(shape(ptr).ne.(/10/))) error stop 7
  if(.not.associated(ptr,tar)) error stop 8

  allocate(tar(50), source=(/(i,i=1,50)/))

  if(lbound(ptr, dim=1).ne. 5) error stop 9
  if(ubound(ptr, dim=1).ne. 14) error stop 10
  if(any(shape(ptr).ne.(/10/))) error stop 11
  if(associated(ptr,tar)) error stop 12

  ptr(25:)=>tar

  if(lbound(ptr, dim=1).ne. 25) error stop 13
  if(ubound(ptr, dim=1).ne. 74) error stop 14
  if(any(shape(ptr).ne.(/50/))) error stop 15
  if(.not.associated(ptr,tar)) error stop 16

  deallocate(tar)

  if(lbound(ptr, dim=1).ne. 25) error stop 17
  if(ubound(ptr, dim=1).ne. 74) error stop 18
  if(any(shape(ptr).ne.(/50/))) error stop 19
  if(.not.associated(ptr,tar)) error stop 20




end