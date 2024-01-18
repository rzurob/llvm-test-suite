!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign015.f
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
!*  TEST CASE TITLE            : ptrassign015
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
!*  DESCRIPTION                :functional testing of bound-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	type ptrtype
     real, pointer :: ptr1(:)
     real, pointer :: ptr2(:,:,:)
  end type
  
  type tartype
    real :: tar1(1:10)
    real :: tar2(1:10, 2:11, 3:12)
  end type

  type(ptrtype) :: ptr
  
  type(tartype), target :: tar
  
  integer :: num1,num2, num3
  
  num1=10
  num2=20
  num3=30
  
  ptr%ptr1((25*5)/(5**2):)=>tar%tar1
  
  if(lbound(ptr%ptr1, dim=1) .ne. 5)  error stop 1
  if(ubound(ptr%ptr1, dim=1) .ne. 14)  error stop 2
  if(any(shape(ptr%ptr1) .ne. shape(tar%tar1)))  error stop 3
  if(.not.associated(ptr%ptr1,tar%tar1))  error stop 4
  
  ptr%ptr2(num1:,num2:,num3:)=>tar%tar2
  
  if(lbound(ptr%ptr2, dim=1) .ne. num1)  error stop 5
  if(lbound(ptr%ptr2, dim=2) .ne. num2)  error stop 6
  if(lbound(ptr%ptr2, dim=3) .ne. num3)  error stop 7
  if(ubound(ptr%ptr2, dim=1) .ne. num1+9)  error stop 8
  if(ubound(ptr%ptr2, dim=2) .ne. num2+9)  error stop 9
  if(ubound(ptr%ptr2, dim=3) .ne. num3+9)  error stop 10
  if(.not.associated(ptr%ptr2,tar%tar2))  error stop 11
  if(any(shape(ptr%ptr2) .ne. shape(tar%tar2)))  error stop 12
  if(.not.associated(ptr%ptr2,tar%tar2))  error stop 13
  
end
  