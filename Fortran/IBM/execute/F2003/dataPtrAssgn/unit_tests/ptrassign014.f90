!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign014.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006s
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bound_spec
!*
!234567890143456789014345678901434567890143456789014345678901434567890

  integer, pointer :: ptr1(:), ptr2(:,:), ptr3(:)
  integer, target :: tar1(10), tar2(1:10, 1:20)

  ptr1(5:)=>tar1

  if(lbound(ptr1, dim=1) .ne. 5)  error stop 1
  if(ubound(ptr1, dim=1) .ne. 14)  error stop 2
  if(any(shape(ptr1) .ne. shape(tar1)))  error stop 3
  if(.not.associated(ptr1,tar1))  error stop 4

  ptr2(10:,20:)=>tar2

  if(lbound(ptr2, dim=1) .ne. 10)  error stop 5
  if(lbound(ptr2, dim=2) .ne. 20)  error stop 6
  if(ubound(ptr2, dim=1) .ne. 19)  error stop 7
  if(ubound(ptr2, dim=2) .ne. 39)  error stop 8
  if(any(shape(ptr2) .ne. shape(tar2)))  error stop 9
  if(.not.associated(ptr2,tar2))  error stop 10

  ptr3(-20:)=>ptr1

  if(lbound(ptr3, dim=1) .ne. -20)  error stop 11
  if(ubound(ptr3, dim=1) .ne. -11)  error stop 12
  if(any(shape(ptr3) .ne. shape(tar1)))  error stop 13
  if(.not.associated(ptr3,ptr1))  error stop 14

end

