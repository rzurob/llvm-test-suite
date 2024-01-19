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

  type dtptr
    real , pointer :: ptr(:,:)
  end type

  type dttar
    real :: tar(15,15)
  end type

  type(dttar) :: arr1

  do i=1,15
    do j=1,15
      arr1%tar(j,i)=i+j
    end do
  end do

  call sub1(arr1%tar(5:10,5:10))

   contains

    subroutine sub1(tar)
      real, target :: tar(:,:)

      type(dtptr) :: dtptr1

      real, pointer :: ptr2

      dtptr1%ptr(2:,3:)=>tar(4:,4:)

      if(lbound(dtptr1%ptr, dim=1).ne. 2) error stop 1
      if(lbound(dtptr1%ptr, dim=2).ne. 3) error stop 2
      if(ubound(dtptr1%ptr, dim=1).ne. 4) error stop 3
      if(ubound(dtptr1%ptr, dim=2).ne. 5) error stop 4
      if(any(shape(dtptr1%ptr).ne.(/3,3/))) error stop 5

      do i=3,5
        do j=2,4
          ptr2=>dtptr1%ptr(j,i)
          if(.not.associated(ptr2,tar(j+2,i+1))) error stop 6
        end do
      end do
    end subroutine

end