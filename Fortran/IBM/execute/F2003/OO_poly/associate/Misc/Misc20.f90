! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20, 2005
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
!*  DESCRIPTION
!*  ICE on implicit logical vars as selector.
!*
!* (ICE-298497 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc20

  IMPLICIT LOGICAL(P)

  ASSOCIATE ( P1 => .true., P2 => P1 .and. P1)
    PRINT*, P1
    PRINT*, P2
    IF (P1 .NEQV. .TRUE. ) STOP 11
  END ASSOCIATE
  END


