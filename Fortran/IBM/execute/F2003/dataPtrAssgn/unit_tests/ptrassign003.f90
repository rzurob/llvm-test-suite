!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign074.f
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
!*  TEST CASE TITLE            : ptrassign074
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


   integer , pointer :: ptr(:,:), ptr2
   integer , target, allocatable :: tar(:)
   integer :: num=1
   
   allocate(tar(10), source=(/(i*i,i=1,10)/))
   
   ptr(5:6,2:6)=>tar
   
  if(lbound(ptr, dim=1).ne. 5) error stop 1
  if(lbound(ptr, dim=2).ne. 2) error stop 2
  if(ubound(ptr, dim=1).ne. 6) error stop 3
  if(ubound(ptr, dim=2).ne. 6) error stop 4
  if(any(shape(ptr).ne.(/2,5/))) error stop 5
   
  do i=2,6
    do j=5,6
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 6
      num=num+1
    end do
  end do
  num=1
  
  deallocate(tar)
  
  if(lbound(ptr, dim=1).ne. 5) error stop 7
  if(lbound(ptr, dim=2).ne. 2) error stop 8
  if(ubound(ptr, dim=1).ne. 6) error stop 9
  if(ubound(ptr, dim=2).ne. 6) error stop 10
  if(any(shape(ptr).ne.(/2,5/))) error stop 11
   
  num=10
  
  allocate(tar(50), source=(/(i,i=1,50)/))
  
  ptr(5:6,2:6)=>tar(num:)

  if(lbound(ptr, dim=1).ne. 5) error stop 13
  if(lbound(ptr, dim=2).ne. 2) error stop 14
  if(ubound(ptr, dim=1).ne. 6) error stop 15
  if(ubound(ptr, dim=2).ne. 6) error stop 16
  if(any(shape(ptr).ne.(/2,5/))) error stop 17
   
  do i=2,6
    do j=5,6
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 18
      num=num+1
    end do
  end do
  num=1
  
  ptr(30:39,40:44)=>tar
  
  if(lbound(ptr, dim=1).ne. 30) error stop 19
  if(lbound(ptr, dim=2).ne. 40) error stop 20
  if(ubound(ptr, dim=1).ne. 39) error stop 21
  if(ubound(ptr, dim=2).ne. 44) error stop 22
  if(any(shape(ptr).ne.(/10,5/))) error stop 23
   
  do i=40,44
    do j=30,39
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 24
      num=num+1
    end do
  end do
  num=1
  
  deallocate(tar)
  
  if(lbound(ptr, dim=1).ne. 30) error stop 25
  if(lbound(ptr, dim=2).ne. 40) error stop 26
  if(ubound(ptr, dim=1).ne. 39) error stop 27
  if(ubound(ptr, dim=2).ne. 44) error stop 28
  if(any(shape(ptr).ne.(/10,5/))) error stop 29
   
end
