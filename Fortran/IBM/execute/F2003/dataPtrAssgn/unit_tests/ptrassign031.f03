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

  type(child), target :: tar1(25)=(/(child(i,i),i=1,25)/)

  class(base), pointer :: ptr(:)

  ptr(25:)=>tar1

  select type (ptr)
    type is (child)

     if(lbound(ptr, dim=1).ne. 25) error stop 1
     if(ubound(ptr, dim=1).ne. 49) error stop 2
     if(any(shape(ptr).ne.(/25/))) error stop 3

   class default
     error stop 7
  end select

  if(.not.associated(ptr,tar1)) error stop 4


end
