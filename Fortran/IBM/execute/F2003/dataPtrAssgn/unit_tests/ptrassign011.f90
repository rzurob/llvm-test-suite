!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C716 functional
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  type dt1
    integer :: num1
  end type

  type, extends(dt1) :: dt1ext
  end type


  class(dt1), pointer :: ptr1(:)
  class(dt1ext), pointer :: ptr2(:)
  type(dt1ext), target :: arr1(1:10)


  ptr2(4:)=>arr1
  if(.not.associated(ptr2,arr1)) error stop 1
  ptr1(5:)=>ptr2(4:)
  if(.not.associated(ptr1,arr1)) error stop 2
  nullify(ptr1)
  ptr1(6:15)=>arr1
  if(.not.associated(ptr1,arr1)) error stop 3


end

