! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign042.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign042.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dtptr(k1)    ! (4)
    integer, kind        :: k1
    integer(k1), pointer :: ptr(:,:)
  end type

  type dttar(n1,k2)    ! (5,4)
     integer, kind :: k2
     integer, len  :: n1
     integer(k2)   :: tar(n1:n1-1)
  end type

  type(dtptr(4)) :: dtptr1

  type(dttar(5,4)), target :: dttar1

  dtptr1%ptr(1:10,1:10)=>dttar1%tar



end

