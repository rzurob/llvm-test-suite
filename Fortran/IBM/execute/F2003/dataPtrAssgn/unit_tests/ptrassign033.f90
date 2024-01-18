!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign033.f
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
!*  TEST CASE TITLE            : ptrassign033
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

  type base
    integer :: num1
  end type
  
  type ,extends(base) :: child
    integer :: num2
  end type
  
  integer :: num=1
  
  class(base), allocatable, target :: tar1(:)
  
  class(*), pointer :: ptr1(:), ptr2(:)
    
  allocate(tar1(30))
  
  
  ptr1(15:)=>tar1

  select type (ptr1)
    type is (base)
     
     if(lbound(ptr1, dim=1).ne. 15) error stop 1
     if(ubound(ptr1, dim=1).ne. 44) error stop 2
     if(any(shape(ptr1).ne.(/30/))) error stop 3
   class default
     error stop 5
  end select
  if(.not.associated(ptr1,tar1)) error stop 4
  
  ptr2(10:)=>ptr1
  
  select type (ptr2)
    type is (base)
     
     if(lbound(ptr2, dim=1).ne. 10) error stop 6
     if(ubound(ptr2, dim=1).ne. 39) error stop 7
     if(any(shape(ptr2).ne.(/30/))) error stop 8
   class default
     error stop 10
  end select
   if(.not.associated(ptr2,tar1)) error stop 9
 
  
end
