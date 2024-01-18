!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptr1assign053.f
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

  integer, pointer :: ptr1(:), ptr2(:)
  integer, target  :: tar1(11:25), tar2(20:30)

  volatile :: ptr1, tar1

  do i=11,25
    tar1(i)=i
  end do

  do i=20,30
    tar2(i)=i
  end do

  !volatile pointer and target
  ptr1(5:)=>tar1

  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(ubound(ptr1, dim=1).ne. 19) error stop 2
  if(any(shape(ptr1).ne.(/15/))) error stop 3
  if(.not.associated(ptr1,tar1)) error stop 4

  !volatile pointer
  ptr1(1:)=>tar2

  if(lbound(ptr1, dim=1).ne. 1) error stop 5
  if(ubound(ptr1, dim=1).ne. 11) error stop 6
  if(any(shape(ptr1).ne.(/11/))) error stop 7
  if(.not.associated(ptr1,tar2)) error stop 8

  !volatile target
  ptr2(25:39)=>tar1

  if(lbound(ptr2, dim=1).ne. 25) error stop 9
  if(ubound(ptr2, dim=1).ne. 39) error stop 10
  if(any(shape(ptr2).ne.(/15/))) error stop 11
  if(.not.associated(ptr2,tar1)) error stop 12

  !nothing volatile
  ptr2(10:20)=>tar2

  if(lbound(ptr2, dim=1).ne. 10) error stop 13
  if(ubound(ptr2, dim=1).ne. 20) error stop 14
  if(any(shape(ptr2).ne.(/11/))) error stop 15
  if(.not.associated(ptr2,tar2)) error stop 16

end