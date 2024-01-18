!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign058.f
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
!*  TEST CASE TITLE            : ptrassign058
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
  
  type(base), pointer :: ptr(:,:), ptr2
  type(base), target, allocatable :: tar(:)
  logical :: l=.true.
  integer :: num=1, lbound1, lbound2, ubound1,ubound2
  
  allocate(tar(225))
  lbound1=10
  lbound2=15
  ubound1=lbound1+15-1
  ubound2=lbound2+15-1
  
  do i=1,225
      tar(i)%data=l
      l=.not.l
  end do
  
  ptr(lbound1:ubound1,lbound2:ubound2)=>tar
  
  if(lbound(ptr, dim=1).ne. 10) error stop 1
  if(lbound(ptr, dim=2).ne. 15) error stop 2
  if(ubound(ptr, dim=1).ne. 24) error stop 3
  if(ubound(ptr, dim=2).ne. 29) error stop 4
  if(any(shape(ptr).ne.(/15,15/))) error stop 5

  do i=15,29
    do j=10,24
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 6
      num=num+1
    end do
  end do

  if(all(ptr%data).neqv.all(tar%data)) error stop 7
  if(any(ptr%data).neqv.any(tar%data)) error stop 8
  if(count(ptr%data).ne.count(tar%data)) error stop 9
  
  
  
  
end