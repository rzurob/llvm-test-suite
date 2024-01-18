!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign044.f
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

  type dt
    integer :: data
  end type

  type(dt), pointer :: ptr1(:,:), ptr2(:)

  type(dt), target :: tar1(100)=(/(dt(i),i=1,100)/)

  ptr1(22:21,22:31)=>tar1

  if(lbound(ptr1,	dim=1).ne. 1) error stop 1
  if(lbound(ptr1, dim=2).ne. 22) error stop 2
 if(ubound(ptr1, dim=1).ne. 0) error stop 3
  if(ubound(ptr1, dim=2).ne. 31) error stop 4
  if(any(shape(ptr1).ne.(/0,10/))) error stop 5

  ptr2(25:24)=>tar1
  if(lbound(ptr2,	dim=1).ne. 1) error stop 6
  if(ubound(ptr2, dim=1).ne. 0) error stop 7
  if(any(shape(ptr2).ne.(/0/))) error stop 8


end