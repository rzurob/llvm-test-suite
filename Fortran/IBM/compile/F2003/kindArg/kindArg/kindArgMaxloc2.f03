!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  characteristics :: the keyword - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc2

  INTEGER(1),    PARAMETER :: I(3) = (/INTEGER(4)::1,1,1/)
  CHARACTER,     PARAMETER :: C(3) = (/character::"I","B","M"/)

  PRINT*, MAXLOC(C, C .EQ. C, KINDD=I(1))
  PRINT*, MAXLOC(ARRAY=C(1), DIM=I(2), KID=kind(1))
  PRINT*, MAXLOC(KIND=I(1),ARRAY=C, ARRAY=C, KIND=2)
  PRINT*, MAXLOC(ARRAY=C, DIM=2, DIM=1, KIND=1_2)

  PRINT*, MAXLOC(C, kInD=I(2)) ! this is fine
  PRINT*, MAXLOC(ARRAY=C, MASK=C .NE. " ", dIM=1, kInD=KIND(C)) ! this is fine

  END



