!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign035.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  logical, target, allocatable :: tar1(:,:)

  logical, pointer :: ptr1(:,:), ptr2(:,:)

  allocate(tar1(10,10))

  ptr1(5:,10:)=>tar1

  if(lbound(ptr1, dim=1).ne. 5)  error stop 1
  if(lbound(ptr1, dim=2).ne. 10) error stop 2
  if(ubound(ptr1, dim=1).ne. 14) error stop 3
  if(ubound(ptr1, dim=2).ne. 19) error stop 4
  if(any(shape(ptr1).ne.(/10,10/))) error stop 5
  if(.not.associated(ptr1,tar1)) error stop 6


  ptr2(-5:,-10:)=>ptr1

  if(lbound(ptr2, dim=1).ne. -5) error stop 7
  if(lbound(ptr2, dim=2).ne. -10) error stop 8
  if(ubound(ptr2, dim=1).ne. 4) error stop 9
  if(ubound(ptr2, dim=2).ne. -1) error stop 10
  if(any(shape(ptr2).ne.(/10,10/))) error stop 11
  if(.not.associated(ptr2,tar1)) error stop 12

end
