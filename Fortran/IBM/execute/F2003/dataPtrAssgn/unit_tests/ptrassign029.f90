!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign029.f
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
!*  TEST CASE TITLE            : ptrassign029
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
  
  class(base), target, allocatable :: tar1(:,:)
  
  type(base), pointer :: ptr1(:,:)
  
  allocate(tar1(10,10))
 
  ptr1(5:,10:)=>tar1
  
  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(lbound(ptr1, dim=2).ne. 10) error stop 2
  if(ubound(ptr1, dim=1).ne. 14) error stop 3
  if(ubound(ptr1, dim=2).ne. 19) error stop 4
  if(any(shape(ptr1).ne.(/10,10/))) error stop 5
  
  select type(tar1)
  type is(base)
      if(.not.associated(ptr1,tar1)) error stop 6
  class default
    error stop 7
  end select
   
 end 
  