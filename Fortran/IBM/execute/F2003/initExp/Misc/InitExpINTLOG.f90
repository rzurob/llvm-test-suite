!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -qintlog
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpINTLOG
  IMPLICIT INTEGER(KIND(IAND((/(J,J=1,16)/),(/(J,J=1,16)/))))(A)
  INTEGER :: I, J

  LOGICAL(1), PARAMETER :: L1T = 1_1 + 0_1
  LOGICAL(1), PARAMETER :: L1F = 1_1 - 1_1

  LOGICAL(2), PARAMETER :: L2T = 1_2 + 0_2
  LOGICAL(2), PARAMETER :: L2F = 1_2 - 1_2

  LOGICAL(4), PARAMETER :: L4T = 1_4 + 0_4
  LOGICAL(4), PARAMETER :: L4F = 1_4 - 1_4

  LOGICAL(8), PARAMETER :: L8T = 1_8 + 0_8
  LOGICAL(8), PARAMETER :: L8F = 1_8 - 1_8


  INTEGER(1),                      PARAMETER :: IArr1(128)=(/(L1T + L1F, I=0, 127)/)

  INTEGER(2),                      PARAMETER :: IArr2(128)=(/(L2T * L2F, I=0, 127)/)

  INTEGER(4),                      PARAMETER :: IArr4(128)=(/(L4T - L4F, I=0, 127)/)

  INTEGER(8),                      PARAMETER :: IArr8(128)=(/(L8F / L8T, I=0, 127)/)


  IF ( L1T .NEQV. .TRUE. )         STOP 10
  IF ( L1F .NEQV. .FALSE.)         STOP 11

  IF ( L2T .NEQV. .TRUE. )         STOP 20
  IF ( L2F .NEQV. .FALSE.)         STOP 21

  IF ( L4T .NEQV. .TRUE. )         STOP 30
  IF ( L4F .NEQV. .FALSE.)         STOP 31

  IF ( L8T .NEQV. .TRUE. )         STOP 40
  IF ( L8F .NEQV. .FALSE.)         STOP 41

  IF ( ANY( IArr1 .NE. 1))         STOP 41
  IF ( ANY( IArr2 .NE. 0))         STOP 41
  IF ( ANY( IArr4 .NE. 1))         STOP 41
  IF ( ANY( IArr8 .NE. 0))         STOP 41

  END


