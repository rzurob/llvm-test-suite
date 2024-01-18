!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign016.f
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
!*  TEST CASE TITLE            : ptrassign016
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
!*  DESCRIPTION                :functional testing of bounds-remapping
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  complex, pointer :: ptr1(:,:), ptr2
  
  complex, target :: tar1(1:20)=(/(cmplx(i,i),i=1,20)/)
  
  integer  :: num1=1
  
  ptr1(21:25,31:34)=>tar1
  
  if(lbound(ptr1,	dim=1).ne. 21) error stop 1
  if(lbound(ptr1, dim=2).ne. 31) error stop 2
  if(ubound(ptr1, dim=1).ne. 25) error stop 3
  if(ubound(ptr1, dim=2).ne. 34) error stop 4
  if(any(shape(ptr1).ne.(/5,4/))) error stop 5

  
  do i=31,34
    do j=21,25
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar1(num1))) error stop 6
      num1=num1+1
    end do
  end do
  
end
  

 