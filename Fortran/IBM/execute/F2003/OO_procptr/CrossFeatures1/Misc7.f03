! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 31, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Data on dup initializion
!* (304715) - take the last value
!*
!234567890123456789012345678901234567890123456789012345678901234567890


! PROGRAM Misc7
! INTEGER A(3)
! DATA A(1:2) /2*1/ A(2:3) /2*2/
! PRINT*, A
! END


  PROGRAM Misc7
  INTEGER A(3), B, C(3)
  DATA A(1:2) /2*1/ A(2:3) /2*2/
  DATA C /3*1/ C /3*2/
  DATA B /1/ B /2/
  PRINT*, A
  PRINT*, C
  PRINT*, B
  IF (ANY(A(1:1) .NE. 1 )) ERROR STOP 11
  IF (ANY(A(2:3) .NE. 2 )) ERROR STOP 12
  IF (ANY(C(1:3) .NE. 2 )) ERROR STOP 13
  IF (B          .NE. 2 )  ERROR STOP 14

  END
