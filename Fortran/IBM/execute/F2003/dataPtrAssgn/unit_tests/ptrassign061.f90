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

  type base
    integer, pointer :: ptr(:,:)
  end type

end module

  use m

  type(base) :: dt1, dt2
  integer, allocatable, target :: tar1(:,:)
  integer :: data=1
  integer :: fsource(7,8)
  logical :: mask(7,8), l=.true.


  allocate(tar1(7,8))


  do i=1,8
    do j=1,7
      tar1(j,i)=data
      data=data+1
    end do
  end do

  do i=1,8
    do j=1,7
      fsource(j,i)=1
    end do
  end do

  do i=1,8
    do j=1,7
      mask(j,i)=l
      l=.false.
    end do
  end do

  dt1%ptr(7:,8:)=>tar1

  if(lbound(dt1%ptr, dim=1).ne. 7) error stop 1
  if(lbound(dt1%ptr, dim=2).ne. 8) error stop 2
  if(ubound(dt1%ptr, dim=1).ne. 13) error stop 3
  if(ubound(dt1%ptr, dim=2).ne. 15) error stop 4
  if(any(shape(dt1%ptr).ne.(/7,8/))) error stop 5
  if(.not.associated(dt1%ptr,tar1)) error stop 6


  if(any(cshift(dt1%ptr,2).ne.cshift(tar1,2))) error stop 7
  if(any(eoshift(dt1%ptr,3).ne.eoshift(tar1,3))) error stop 8
  if(any(merge(dt1%ptr,fsource,mask).ne.merge(tar1,fsource,mask))) error stop 9
  if(any(transpose(dt1%ptr).ne.transpose(tar1))) error stop 10
  if(any(pack(dt1%ptr,mask).ne.pack(tar1,mask))) error stop 11
  if(any(reshape(dt1%ptr,(/56/)).ne.reshape(tar1,(/56/)))) error stop 12
  if(any(spread(dt1%ptr,2,2).ne.spread(tar1,2,2))) error stop 13
  if(any(unpack(dt1%ptr(11,:),mask,0).ne.unpack(tar1(5,:),mask,0))) error stop 14





end


