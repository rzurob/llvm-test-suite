!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign037.f
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
!*  TEST CASE TITLE            : ptrassign037
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

  complex :: arr(9,9)
  
  do i=1,9
    do j=1,9
      arr(j,i)=cmplx(j,i)
    end do
  end do
  
  arr=func1(arr)

  contains
    
    function func1(tar1)
      complex, target :: tar1(:,:)
      
      complex :: func1(9,9)
      
      complex, pointer :: ptr1(:,:), ptr2
      
      ptr1(3:,-3:)=>tar1
      
      if(lbound(ptr1, dim=1).ne. 3) error stop 1
      if(lbound(ptr1, dim=2).ne. -3) error stop 2
      if(ubound(ptr1, dim=1).ne. 11) error stop 3
      if(ubound(ptr1, dim=2).ne. 5) error stop 4
      if(any(shape(ptr1).ne.(/9,9/))) error stop 5
  
      do i=-3,5
        do j=3,11
          ptr2=>ptr1(j,i)
          if(.not.associated(ptr2,tar1(j-2,i+4))) error stop 6
        end do
      end do
      
      func1=ptr1
      
    end function

end

