!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign064.f
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
!*  TEST CASE TITLE            : ptrassign064
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
      integer :: data
    end type
    
    type base2
      type(base), pointer :: ptr(:,:)
    end type
    
    type container
      type(base2) :: b2
    end type
    
  end module
  
  use m
  
  type(container) :: c1
  type(base), target :: tar(50)=(/(base(i),i=1,50)/)
  type(base), pointer :: ptr2
  integer :: num, lowerb1,lowerb2,upperb1,upperb2
  
  lowerb1=int(25.0)
  lowerb2=lowerb1+int(sqrt(real(lowerb1)))
  upperb1=lowerb2+4
  upperb2=upperb1
  c1%b2%ptr(lowerb1:upperb1,lowerb2:upperb2)=>tar
  
  associate(x=>c1%b2)
    num=1
    if(lbound(x%ptr, dim=1).ne. 25) error stop 1
    if(lbound(x%ptr, dim=2).ne. 30) error stop 2
    if(ubound(x%ptr, dim=1).ne. 34) error stop 3
    if(ubound(x%ptr, dim=2).ne. 34) error stop 4
    if(any(shape(x%ptr).ne.(/10,5/))) error stop 5

    do i=30,34
      do j=25,34
        ptr2=>x%ptr(j,i)
        if(.not.associated(ptr2,tar(num))) error stop 6
        num=num+1
      end do
    end do
    num=1    
  end associate
  
    if(lbound(c1%b2%ptr, dim=1).ne. 25) error stop 7
    if(lbound(c1%b2%ptr, dim=2).ne. 30) error stop 8
    if(ubound(c1%b2%ptr, dim=1).ne. 34) error stop 9
    if(ubound(c1%b2%ptr, dim=2).ne. 34) error stop 10
    if(any(shape(c1%b2%ptr).ne.(/10,5/))) error stop 11

    do i=30,34
      do j=25,34
        ptr2=>c1%b2%ptr(j,i)
        if(.not.associated(ptr2,tar(num))) error stop 12
        num=num+1
      end do
    end do
  
end program

    
