!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign059.f
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
!*  TEST CASE TITLE            : ptrassign059
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

  real, pointer :: ptr(:,:,:)
  
  real, target :: tar(5,5,5)
  
  real :: num=1.0
  
  
  do i=1,5
    do j=1,5
      do k=1,5
        tar(k,j,i)=sin(num)
        num=num+1.0
      end do
    end do
  end do
  
  ptr(11:,21:,31:)=>tar
  
  if(lbound(ptr, dim=1).ne. 11) error stop 1
  if(lbound(ptr, dim=2).ne. 21) error stop 2
  if(lbound(ptr, dim=3).ne. 31) error stop 3
  if(ubound(ptr, dim=1).ne. 15) error stop 4
  if(ubound(ptr, dim=2).ne. 25) error stop 5
  if(ubound(ptr, dim=3).ne. 35) error stop 6
  if(any(shape(ptr).ne.(/5,5,5/))) error stop 7
  if(.not.associated(ptr,tar)) error stop 8
  
  if(maxval(ptr).ne.maxval(tar)) error stop 9
  if(minval(ptr).ne.minval(tar)) error stop 10
  if(any(maxloc(ptr).ne.maxloc(tar))) error stop 11
  if(any(minloc(ptr).ne.minloc(tar))) error stop 12
  if(size(ptr).ne. size(tar)) error stop 13
  if(product(ptr).ne.product(tar)) error stop 14
  if(sum(ptr).ne.sum(tar)) error stop 15

end
         