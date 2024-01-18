!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign020.f
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
!*  TEST CASE TITLE            : ptrassign020
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
  
  integer :: bound1, bound2, num1
  
  real, target :: tar1(10)=(/(i,i=1,10)/)
  
  bound1=9
  bound2=10
  num1=1
  
  ptr1(int(log10(10.0))+4:min(bound1,bound2),max(bound1,bound2):int(1.0)*11)=>tar1
  
  ptr2((int(-1.0)*(-1))+4:)=>tar1
  
  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(lbound(ptr1, dim=2).ne. 10) error stop 2
  if(ubound(ptr1, dim=1).ne. 9) error stop 3
  if(ubound(ptr1, dim=2).ne. 11) error stop 4
  if(any(shape(ptr1).ne.(/5,2/))) error stop 5
  if(loc(ptr1).ne.loc(tar1)) error stop 6
  
  do i=10,11
    do j=5,9
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num1))) error stop 7
      num1=num1+1
    end do
  end do
  
  if(lbound(ptr2, dim=1) .ne. 5)  error stop 8
  if(ubound(ptr2, dim=1) .ne. 14)  error stop 9
  if(any(shape(ptr2) .ne. shape(tar1)))  error stop 10
  if(.not.associated(ptr2,tar1))  error stop 11
  
end  
  
  

  	