!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign022a.f
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
!*  TEST CASE TITLE            : ptrassign022a
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

  real, pointer :: ptr1(:,:), ptr2(:), ptr3
  
  integer :: num1=4
  
  real, target :: tar1(25)= (/(real(i),i=1,25)/)
  
  ptr1(12:15,16:19)=>tar1(4:19)
  
  if(lbound(ptr1, dim=1).ne. 12) error stop 1
  if(lbound(ptr1, dim=2).ne. 16) error stop 2
  if(ubound(ptr1, dim=1).ne. 15) error stop 3
  if(ubound(ptr1, dim=2).ne. 19) error stop 4
  if(any(shape(ptr1).ne.(/4,4/))) error stop 5
  
  do i=16,19
    do j=12,15
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num1))) error stop 7
      num1=num1+1
    end do
  end do
  
  ptr2(11:20)=>tar1(12:21)
  
  if(lbound(ptr2, dim=1).ne. 11) error stop 8
  if(ubound(ptr2, dim=1).ne. 20) error stop 9
  if(any(shape(ptr2).ne.(/10/))) error stop 10
  
  num1=12
  do i=11,20
    ptr3=>ptr2(i)
    if(.not.associated(ptr3,tar1(num1))) error stop 11
    num1=num1+1
  end do

end
      
