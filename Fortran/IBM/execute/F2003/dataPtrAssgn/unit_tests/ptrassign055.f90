!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptr1assign055.f
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

  real, pointer :: ptr1(:), ptr2(:)
  real, target  :: tar1(11:25), tar2(20:30)

  asynchronous :: ptr1, tar1

  do i=11,25
    tar1(i)=real(i)
  end do

  do i=20,30
    tar2(i)=real(i)
  end do

  !asynchronus pointer and target
  ptr1(int(5.0):)=>tar1

  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(ubound(ptr1, dim=1).ne. 19) error stop 2
  if(any(shape(ptr1).ne.(/15/))) error stop 3
  if(.not.associated(ptr1,tar1)) error stop 4

  !asynchronus pointer
  ptr1(int(exp(cmplx(0,1)*acos(-1.0)))*(-1):)=>tar2

  if(lbound(ptr1, dim=1).ne. 1) error stop 5
  if(ubound(ptr1, dim=1).ne. 11) error stop 6
  if(any(shape(ptr1).ne.(/11/))) error stop 7
  if(.not.associated(ptr1,tar2)) error stop 8

  !asynchronus target
  ptr2(1+2+3+4+5+10:3*13)=>tar1

  if(lbound(ptr2, dim=1).ne. 25) error stop 9
  if(ubound(ptr2, dim=1).ne. 39) error stop 10
  if(any(shape(ptr2).ne.(/15/))) error stop 11
  if(.not.associated(ptr2,tar1)) error stop 12

  !nothing asynchronus
  ptr2(10:20)=>tar2

  if(lbound(ptr2, dim=1).ne. 10) error stop 13
  if(ubound(ptr2, dim=1).ne. 20) error stop 14
  if(any(shape(ptr2).ne.(/11/))) error stop 15
  if(.not.associated(ptr2,tar2)) error stop 16

end