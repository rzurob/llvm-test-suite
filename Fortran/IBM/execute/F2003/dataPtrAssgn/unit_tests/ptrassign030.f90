!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign030.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type base
    integer :: num1
  end type

  type ,extends(base) :: child
    integer :: num2
  end type

  class(base), allocatable, target :: tar1(:,:,:)

  class(base), pointer :: ptr(:,:,:)

  allocate(tar1(3,4,5))


  ptr(0:,-1:, -2: )=>tar1

  select type (ptr)
    type is (base)

     if(lbound(ptr, dim=1).ne. 0) error stop 1
     if(lbound(ptr, dim=2).ne. -1) error stop 2
     if(lbound(ptr, dim=3).ne. -2) error stop 3
     if(ubound(ptr, dim=1).ne. 2) error stop 4
     if(ubound(ptr, dim=2).ne. 2) error stop 5
     if(ubound(ptr, dim=3).ne. 2) error stop 6
     if(any(shape(ptr).ne.(/3,4,5/))) error stop 7


   class default
     error stop 9
  end select

  if(.not.associated(ptr,tar1)) error stop 10


end

