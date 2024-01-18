!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign048.f
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
!*  TEST CASE TITLE            : ptrassign048
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
  
  type(dt1), pointer :: ptr1(:), ptr2(:)
  
  type(dt1), allocatable,target :: tar(:)
  
  integer :: lowerb

  allocate(tar(20), source=(/(dt1(i),i=1,20)/))
  
  lowerb=10
  
  ptr1(lowerb:)=>tar(5:14)
  
  if(lbound(ptr1, dim=1).ne. 10) error stop 1
  if(ubound(ptr1, dim=1).ne. 19) error stop 2
  if(any(shape(ptr1).ne.(/10/))) error stop 3
  if(.not.associated(ptr1,tar(5:14))) error stop 4

  lowerb=-5
  ptr2(lowerb:)=>ptr1
  
  if(lbound(ptr2, dim=1).ne. -5) error stop 5
  if(ubound(ptr2, dim=1).ne. 4) error stop 6
  if(any(shape(ptr2).ne.(/10/))) error stop 7
  if(.not.associated(ptr2,ptr1)) error stop 8
 
end 
  
  
