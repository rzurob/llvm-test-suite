!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptr1assign057.f
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
!*  TEST CASE TITLE            : ptrassign057
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

module m
  
  type base
    logical :: data
  end type
  
end module

  use m
  
  type(base), pointer :: ptr(:,:)
  type(base), target, allocatable :: tar(:,:)
  logical :: l=.true.
  
  allocate(tar(15,15))
  
  do i=1,15
    do j=1,15
      tar(j,i)%data=l
      l=.not.l
    end do
  end do
  
  ptr(1:,1:)=>tar(5:9,5:9)  
  
  if(lbound(ptr, dim=1).ne. 1) error stop 1
  if(lbound(ptr, dim=2).ne. 1) error stop 2
  if(ubound(ptr, dim=1).ne. 5) error stop 3
  if(ubound(ptr, dim=2).ne. 5) error stop 4
  if(any(shape(ptr).ne.(/5,5/))) error stop 5
  if(.not.associated(ptr,tar(5:9, 5:9))) error stop 6
  
  if(all(ptr%data).neqv.all(tar(5:9,5:9)%data)) error stop 7
  if(any(ptr%data).neqv.any(tar(5:9,5:9)%data)) error stop 8
  if(count(ptr%data).ne.count(tar(5:9,5:9)%data)) error stop 9
  
  
  
  
end
