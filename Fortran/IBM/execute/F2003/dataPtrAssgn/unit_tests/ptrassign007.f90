!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign070.f
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


integer, pointer :: ptr1(:,:), ptr2
integer, target  :: tar1(100)
integer          :: num=1

interface
  subroutine ptrassign(ptr,tar)
    integer , pointer :: ptr(:,:)
    integer, target  :: tar(:)
  end subroutine
end interface
procedure(ptrassign), pointer :: procptr

procptr=>ptrassign

do i=1,100
  tar1(i)=100
end do

call procptr(ptr1,tar1)

  if(lbound(ptr1, dim=1).ne. 1) error stop 7
  if(lbound(ptr1, dim=2).ne. 20) error stop 8
  if(ubound(ptr1, dim=1).ne. 5) error stop 9
  if(ubound(ptr1, dim=2).ne. 39) error stop 10
  if(any(shape(ptr1).ne.(/5,20/))) error stop 11

  do i=20,39
    do j=1,5
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar1(num))) error stop 12
      num=num+1
    end do
  end do



end


subroutine ptrassign(ptr,tar)
  integer, pointer :: ptr(:,:), ptr2
  integer, target  :: tar(:)
  integer          :: num=1

  ptr(1:5,20:39)=>tar

  if(lbound(ptr, dim=1).ne. 1) error stop 1
  if(lbound(ptr, dim=2).ne. 20) error stop 2
  if(ubound(ptr, dim=1).ne. 5) error stop 3
  if(ubound(ptr, dim=2).ne. 39) error stop 4
  if(any(shape(ptr).ne.(/5,20/))) error stop 5

  do i=20,39
    do j=1,5
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 6
      num=num+1
    end do
  end do



end subroutine

