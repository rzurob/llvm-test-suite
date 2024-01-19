!****************************************************************
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

module m

  contains

    subroutine sub1(ptr,tar)
      integer, pointer, intent(in) :: ptr(:,:)
      integer, target :: tar(:,:)

      if(lbound(ptr, dim=1).ne. 100) error stop 1
      if(lbound(ptr, dim=2).ne. 200) error stop 2
      if(ubound(ptr, dim=1).ne. 103) error stop 3
      if(ubound(ptr, dim=2).ne. 204) error stop 4
      if(any(shape(ptr).ne.(/4,5/))) error stop 5
      if(.not.associated(ptr,tar)) error stop 6

    end subroutine

    subroutine sub2(ptr,tar)
      integer, pointer, intent(out) :: ptr(:,:)
      integer, target, intent(in) :: tar(:,:)

      ptr(11:,21:)=>tar

      if(lbound(ptr, dim=1).ne. 11) error stop 7
      if(lbound(ptr, dim=2).ne. 21) error stop 8
      if(ubound(ptr, dim=1).ne. 14) error stop 9
      if(ubound(ptr, dim=2).ne. 25) error stop 10
      if(any(shape(ptr).ne.(/4,5/))) error stop 11
      if(.not.associated(ptr,tar)) error stop 12

    end subroutine

    subroutine sub3(ptr, tar)
      integer, pointer, intent(inout) :: ptr(:,:)
      integer, target, intent(in) :: tar(:,:)

      if(lbound(ptr, dim=1).ne. -4) error stop 19
      if(lbound(ptr, dim=2).ne. -5) error stop 20
      if(ubound(ptr, dim=1).ne. -1) error stop 21
      if(ubound(ptr, dim=2).ne. -1) error stop 22
      if(any(shape(ptr).ne.(/4,5/))) error stop 23
      if(.not.associated(ptr,tar)) error stop 24

      ptr=>null()
      ptr(25:,25:)=>tar

      if(lbound(ptr, dim=1).ne. 25) error stop 25
      if(lbound(ptr, dim=2).ne. 25) error stop 26
      if(ubound(ptr, dim=1).ne. 28) error stop 27
      if(ubound(ptr, dim=2).ne. 29) error stop 28
      if(any(shape(ptr).ne.(/4,5/))) error stop 29
      if(.not.associated(ptr,tar)) error stop 30


    end subroutine



end module


  use m

  integer, pointer :: ptr1(:,:)
  integer, target :: tar1(4,5)

  do i=1,5
    do j=1,4
      tar1(j,i)=1
    end do
  end do

  ptr1(100:,200:)=>tar1

  call sub1(ptr1, tar1)

  ptr1=>null()

  call sub2(ptr1, tar1)

  if(lbound(ptr1, dim=1).ne. 11) error stop 13
  if(lbound(ptr1, dim=2).ne. 21) error stop 14
  if(ubound(ptr1, dim=1).ne. 14) error stop 15
  if(ubound(ptr1, dim=2).ne. 25) error stop 16
  if(any(shape(ptr1).ne.(/4,5/))) error stop 17
  if(.not.associated(ptr1,tar1)) error stop 18

  ptr1=>null()

  ptr1(-4:,-5:)=>tar1
  call sub3(ptr1, tar1)

  if(lbound(ptr1, dim=1).ne. 25) error stop 31
  if(lbound(ptr1, dim=2).ne. 25) error stop 32
  if(ubound(ptr1, dim=1).ne. 28) error stop 33
  if(ubound(ptr1, dim=2).ne. 29) error stop 34
  if(any(shape(ptr1).ne.(/4,5/))) error stop 35
  if(.not.associated(ptr1,tar1)) error stop 36

end program

