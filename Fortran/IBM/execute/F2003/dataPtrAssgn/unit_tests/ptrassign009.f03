!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :C724
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  integer, pointer :: ptr1(:)

  integer, target :: num1(10)=(/1,1,1,1,1,1,1,1,1,1/)

  !function returns pointer, okay
  ptr1(5:)=>func1(num1)
  if(.not.associated(ptr1,num1)) error stop 1

  nullify(ptr1)

  ptr1(5:14)=>func1(num1)
  if(.not.associated(ptr1,num1)) error stop 2

  contains

    function func1(arg1)
      integer, target :: arg1(10)
      integer, pointer :: func1(:)

      func1(2:)=>arg1
    end function

end
