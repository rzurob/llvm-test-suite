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
    integer, pointer :: ptr(:,:,:)
  end type

  type dttar
     integer :: tar(5:4,0:2,1:0)
  end type

  type(dtptr) :: dtptr1

  type(dttar), target :: dttar1

  dtptr1%ptr(10:,20:,30:)=>dttar1%tar



end

