!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign034.f
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

  logical, target, allocatable :: tar1(:)

  logical, pointer :: ptr1(:,:), ptr2(:,:), ptr3

  integer :: num=1

  allocate(tar1(15))

  ptr2(3:5,3:7)=>tar1

  if(lbound(ptr2, dim=1).ne. 3) error stop 1
  if(lbound(ptr2, dim=2).ne. 3) error stop 2
  if(ubound(ptr2, dim=1).ne. 5) error stop 3
  if(ubound(ptr2, dim=2).ne. 7) error stop 4
  if(any(shape(ptr2).ne.(/3,5/))) error stop 5

  do i=3,7
    do j=3,5
      ptr3=>ptr2(j,i)
      if(.not.associated(ptr3,tar1(num))) error stop 6
      num=num+1
    end do
  end do

  ptr1(5:,5:)=>ptr2

  if(lbound(ptr1, dim=1).ne. 5) error stop 7
  if(lbound(ptr1, dim=2).ne. 5) error stop 8
  if(ubound(ptr1, dim=1).ne. 7) error stop 9
  if(ubound(ptr1, dim=2).ne. 9) error stop 10
  if(any(shape(ptr1).ne.(/3,5/))) error stop 11

  num=1
  do i=5,9
    do j=5,7
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num))) error stop 12
      num=num+1
    end do
  end do


end
