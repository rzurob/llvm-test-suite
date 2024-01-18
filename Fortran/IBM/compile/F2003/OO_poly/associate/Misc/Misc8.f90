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
!*  Intrinsic type as type guard caused ICE and extra Err Msg.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  TYPE t
  END TYPE

  CLASS(t), ALLOCATABLE :: Y

  ALLOCATE(Y)
  SELECT TYPE (X => Y)
    type is (integer)
      print*, "wrong!"
  END SELECT

  END

