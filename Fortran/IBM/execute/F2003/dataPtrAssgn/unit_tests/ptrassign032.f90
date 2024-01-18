!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
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

  class(base), allocatable, target :: tar1(:,:)

  class(base), pointer :: ptr(:,:)

  allocate(tar1(5,6))


  ptr(2:,3:)=>tar1(1:4,2:5)

  select type (ptr)
    type is (base)

     if(lbound(ptr, dim=1).ne. 2) error stop 1
     if(lbound(ptr, dim=2).ne. 3) error stop 2
     if(ubound(ptr, dim=1).ne. 5) error stop 3
     if(ubound(ptr, dim=2).ne. 6) error stop 4
     if(any(shape(ptr).ne.(/4,4/))) error stop 5


   class default
     error stop 7
  end select

  if(.not.associated(ptr,tar1(1:4,2:5))) error stop 6


end
