! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign006d.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                :C720
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dtptr(k1)    ! (4)
    integer, kind        :: k1
    integer(k1), pointer :: ptr1(:,:,:,:)
    integer(k1), pointer :: ptr2(:,:)
  end type

  integer, target :: arr1(1:5,2:6,3:7)

  type(dtptr(4)) :: dtptr1

  !rank or pointer does not match rank of target
  dtptr1%ptr1(4:8,5:9,6:10,11:11)=>arr1

  !rank of pointer does not match rank of traget
  dtptr1%ptr2(4:8,5:9)=>arr1


end

