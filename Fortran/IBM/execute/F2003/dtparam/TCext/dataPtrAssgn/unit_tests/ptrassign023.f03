! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign023.f
! opt variations: -qnol

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

  type dt(n1,k1)    ! (20,4)
    integer, kind     :: k1
    integer, len      :: n1
    real(k1), pointer :: ptr1(:,:,:)
  end type

  type(dt(20,4)):: dt1

  real, target :: tar1(5,5,5)

  integer :: lbound1, lbound2, lbound3

  lbound1=4
  lbound2=8
  lbound3=12

  dt1%ptr1(lbound1:,lbound2:,lbound3:)=>tar1(2:,3:,2:)

  if(lbound(dt1%ptr1, dim=1).ne. 4) error stop 1
  if(lbound(dt1%ptr1, dim=2).ne. 8) error stop 2
  if(lbound(dt1%ptr1, dim=3).ne. 12) error stop 3
  if(ubound(dt1%ptr1, dim=1).ne. 7) error stop 4
  if(ubound(dt1%ptr1, dim=2).ne. 10) error stop 5
  if(ubound(dt1%ptr1, dim=3).ne. 15) error stop 6
  if(any(shape(dt1%ptr1).ne.shape(tar1(2:,3:,2:)))) error stop 7
  if(.not.associated(dt1%ptr1,tar1(2:,3:,2:))) error stop 8

end

