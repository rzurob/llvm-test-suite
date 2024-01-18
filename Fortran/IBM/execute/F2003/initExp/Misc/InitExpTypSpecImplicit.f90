!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 30, 2006
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
!*  Type Spec in IMPLICIT STMT
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM InitExpTypSpecImplicit

  IMPLICIT INTEGER(KIND=1_8) (A)
  IMPLICIT INTEGER(KIND=2_4) (B)
  IMPLICIT INTEGER(KIND=4_2) (C)
  IMPLICIT INTEGER(KIND=8_1) (D)

  IMPLICIT LOGICAL(KIND=KIND((/0_1/))) (E)
  IMPLICIT LOGICAL(KIND=KIND((/0_2/))) (F)
  IMPLICIT LOGICAL(KIND=KIND((/0_4/))) (G)
  IMPLICIT LOGICAL(KIND=KIND((/0_8/))) (H)

  INTEGER, PARAMETER :: Res(128)=(/(-I, I=1, 128)/)
  LOGICAL, PARAMETER :: LRes1(128)=(/(.TRUE., I=1, 128)/)
  LOGICAL, PARAMETER :: LRes2(128)=(/(.FALSE., I=1, 128)/)

  DIMENSION A(128)
  PARAMETER (A=(/(-I, I=1, 128)/))
  DIMENSION B(128)
  PARAMETER (B=(/(-I, I=1, 128)/))
  DIMENSION C(128)
  PARAMETER (C=(/(-I, I=1, 128)/))
  DIMENSION D(128)
  PARAMETER (D=(/(-I, I=1, 128)/))

  DIMENSION E(128)
  PARAMETER (E=(/(.TRUE., I=1, 128)/))
  DIMENSION F(128)
  PARAMETER (F=(/(.TRUE., I=1, 128)/))
  DIMENSION G(128)
  PARAMETER (G=(/(.FALSE., I=1, 128)/))
  DIMENSION H(128)
  PARAMETER (H=(/(.FALSE., I=1, 128)/))

  IF ( KIND(A) .NE. 1     ) STOP 11
  IF ( ANY(A   .NE. Res ) ) STOP 12
  IF ( KIND(B) .NE. 2     ) STOP 13
  IF ( ANY(B   .NE. Res ) ) STOP 14
  IF ( KIND(C) .NE. 4     ) STOP 15
  IF ( ANY(C   .NE. Res ) ) STOP 16
  IF ( KIND(D) .NE. 8     ) STOP 17
  IF ( ANY(D   .NE. Res ) ) STOP 18

  IF ( KIND(E) .NE. 1       )   STOP 21
  IF ( ANY(E   .NEQV. LRes1 ) ) STOP 22
  IF ( KIND(F) .NE. 2       )   STOP 23
  IF ( ANY(F   .NEQV. LRes1 ) ) STOP 24
  IF ( KIND(G) .NE. 4       )   STOP 25
  IF ( ANY(G   .NEQV. LRes2 ) ) STOP 26
  IF ( KIND(H) .NE. 8       )   STOP 27
  IF ( ANY(H   .NEQV. LRes2 ) ) STOP 28

  END


