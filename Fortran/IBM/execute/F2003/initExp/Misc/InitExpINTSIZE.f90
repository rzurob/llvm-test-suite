!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qintsize
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -qintsize
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpINTSIZE
  IMPLICIT NONE

  INTEGER :: I
  LOGICAL :: L

  INTEGER,                           PARAMETER :: IArr1(128)=(/(7, I=0, 127)/)
  INTEGER,                           PARAMETER :: IArr2(128)=(/(6, I=0, 127)/)
  INTEGER(KIND(IEOR(IArr1, IArr2))), PARAMETER :: IArr(128) =IEOR(IArr1, IArr2)

  LOGICAL,                           PARAMETER :: LArr1(128)=(/(.FALSE., .TRUE., I=0, 127, 2)/)
  LOGICAL,                           PARAMETER :: LArr2(128)=(/(.TRUE., .FALSE., I=0, 127, 2)/)
  LOGICAL(KIND(LArr1 .OR. LArr2)),   PARAMETER :: LArr(128) = LArr1 .OR. LArr2


  IF ( KIND(IArr) .NE. KIND(I))  ERROR STOP 11
  IF ( ANY( IArr  .NE. 1 ))      ERROR STOP 12

  IF ( KIND(LArr) .NE. KIND(I))  ERROR STOP 21
  IF ( ANY( LArr  .NEQV. .TRUE.))ERROR STOP 22


  END


