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
!*  DESCRIPTION                :C719
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt
    integer :: num1
  end type

  type(dt) , pointer :: ptr1(:,:,:)

  type(dt), target :: arr2(1:10)

  !incorrect rank for pointer
  ptr1(4:8,5:9,6:10,7:11)=>arr1

  ptr1(4:8,5:9)=>arr1

  ptr1(4:8,5:9,6:10,7:11)=>arr1

  ptr1(4:8,5:9)=>arr1


end

