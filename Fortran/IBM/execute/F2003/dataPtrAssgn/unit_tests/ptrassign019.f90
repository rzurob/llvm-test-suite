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
!*  DESCRIPTION                :functional testing of bounds-remapping
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  integer, pointer :: ptr1(:,:,:), ptr2

  integer :: bounds1, bounds2, bounds3

  integer , target :: tar1(27)=(/(i,i=1,27)/)

  integer :: num1=1

  bounds1=15
  bounds2=18
  bounds3=21

  ptr1(bounds1:bounds1+(8-5+1-2),bounds2:bounds1+(5**1),bounds3:(bounds2*2)-13)=>tar1

  if(lbound(ptr1, dim=1).ne. 15) error stop 1
  if(lbound(ptr1, dim=2).ne. 18) error stop 2
  if(lbound(ptr1, dim=3).ne. 21) error stop 3
  if(ubound(ptr1, dim=1).ne. 17) error stop 4
  if(ubound(ptr1, dim=2).ne. 20) error stop 5
  if(ubound(ptr1, dim=3).ne. 23) error stop 6
  if(any(shape(ptr1).ne.(/3,3,3/))) error stop 9


  do i=21,23
    do j=18,20
      do k=15,17
        ptr2=>ptr1(k,j,i)
        if(.not.associated(ptr2,tar1(num1))) error stop 11
        num1=num1+1
      end do
    end do
  end do

end