! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign043.f
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

  type dtptr(n1,k1)    ! (20,4)
    integer, kind        :: k1
    integer, len         :: n1
    integer(k1), pointer :: ptr(:,:,:)
  end type

  type dttar(n2,k2)    ! (20,4)
     integer, kind :: k2
     integer, len  :: n2
     integer(k2)   :: tar(5:4,0:2,1:0)
  end type

  type(dtptr(20,4)) :: dtptr1

  type(dttar(20,4)), target :: dttar1

  dtptr1%ptr(10:,20:,30:)=>dttar1%tar



end

