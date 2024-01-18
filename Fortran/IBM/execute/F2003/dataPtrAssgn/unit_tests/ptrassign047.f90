!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign047.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ptrassign047
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt1
    integer :: data
  end type
  
  type(dt1), pointer :: ptr1(:,:), ptr2, ptr3(:,:)
  
  type(dt1), allocatable,target :: tar(:)
  
  integer :: num=5
  
  allocate(tar(20), source=(/(dt1(i),i=1,20)/))
  
  ptr1(2:6,2:3)=>tar(5:14)
  
  if(lbound(ptr1, dim=1).ne. 2) error stop 1
  if(lbound(ptr1, dim=2).ne. 2) error stop 2
  if(ubound(ptr1, dim=1).ne. 6) error stop 3
  if(ubound(ptr1, dim=2).ne. 3) error stop 4
  if(any(shape(ptr1).ne.(/5,2/))) error stop 5
      
  do i=2,3
    do j=2,6
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 6
      num=num+1
    end do
  end do
  
  ptr3(10:,15:)=>ptr1
  
  if(lbound(ptr3, dim=1).ne. 10) error stop 7
  if(lbound(ptr3, dim=2).ne. 15) error stop 8
  if(ubound(ptr3, dim=1).ne. 14) error stop 9
  if(ubound(ptr3, dim=2).ne. 16) error stop 10
  if(any(shape(ptr3).ne.(/5,2/))) error stop 11
      
  num=5
  do i=15,15
    do j=10,12
      ptr2=>ptr3(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 12
      num=num+1
    end do
  end do
  
end 
  
  
