! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/associate/Misc/Misc9.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTIOa
!*
!*  Unknown entity as type guard of select type caused ICE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  TYPE t(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(t(4,20)), ALLOCATABLE :: Y

  ALLOCATE(Y)
  SELECT TYPE ( Y)
    type is (W) ! Unknown entity
      print*, "wrong!"
  END SELECT

  END
