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
      integer :: data
      contains
        procedure, nopass :: assign=>ptrassign
    end type
    contains

      subroutine ptrassign(ptr, tar)
        type(base), pointer :: ptr(:,:)
        type(base), target  :: tar(:,:)

        ptr(6:,6:)=>tar(1:5,1:5)

        if(lbound(ptr, dim=1).ne. 6) error stop 1
        if(lbound(ptr, dim=2).ne. 6) error stop 2
        if(ubound(ptr, dim=1).ne. 10) error stop 3
        if(ubound(ptr, dim=2).ne. 10) error stop 4
        if(any(shape(ptr).ne.(/5,5/))) error stop 5
        if(.not.associated(ptr,tar(1:5,1:5))) error stop 6

      end subroutine


  end module

  use m

  type(base), pointer :: ptr1(:,:)
  type(base), target, allocatable  :: tar1(:,:)

  allocate(tar1(8,6), source=base(1))

  call tar1%assign(ptr1,tar1)

  if(lbound(ptr1, dim=1).ne. 6) error stop 7
  if(lbound(ptr1, dim=2).ne. 6) error stop 8
  if(ubound(ptr1, dim=1).ne. 10) error stop 9
  if(ubound(ptr1, dim=2).ne. 10) error stop 10
  if(any(shape(ptr1).ne.(/5,5/))) error stop 11
  if(.not.associated(ptr1,tar1(1:5,1:5))) error stop 12



end