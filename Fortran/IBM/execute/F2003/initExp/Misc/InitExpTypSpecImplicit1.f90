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
!*  type-spec in implicit stmt
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpeImplicit1


  IMPLICIT REAL(KIND=4_8)  (A)
  IMPLICIT REAL(KIND=8_4)  (B)
  IMPLICIT REAL(KIND=16_2) (C)

  IMPLICIT COMPLEX(KIND=KIND((/0.E0/))) (E)
  IMPLICIT COMPLEX(KIND=KIND((/0.D0/))) (F)
  IMPLICIT COMPLEX(KIND=KIND((/0.Q0/))) (G)

  REAL, PARAMETER :: Res(128)=(/(-I, I=1, 128)/)

  DIMENSION A(128)
  PARAMETER (A=(/(-I, I=1, 128)/))
  DIMENSION B(128)
  PARAMETER (B=(/(-I, I=1, 128)/))
  DIMENSION C(128)
  PARAMETER (C=(/(-I, I=1, 128)/))

  DIMENSION E(128)
  PARAMETER (E=(/((-I,0), I=1, 128)/))
  DIMENSION F(128)
  PARAMETER (F=(/((-I,0), I=1, 128)/))
  DIMENSION G(128)
  PARAMETER (G=(/((-I,0), I=1, 128)/))

  IF ( KIND(A) .NE. 4     ) ERROR STOP 11
  IF ( ANY(A   .NE. Res ) ) ERROR STOP 12
  IF ( KIND(B) .NE. 8     ) ERROR STOP 13
  IF ( ANY(B   .NE. Res ) ) ERROR STOP 14
  IF ( KIND(C) .NE. 16    ) ERROR STOP 15
  IF ( ANY(C   .NE. Res ) ) ERROR STOP 16

  IF ( KIND(E) .NE. 4     ) ERROR STOP 21
  IF ( ANY(E   .NE. Res ) ) ERROR STOP 22
  IF ( KIND(F) .NE. 8     ) ERROR STOP 23
  IF ( ANY(F   .NE. Res ) ) ERROR STOP 24
  IF ( KIND(G) .NE. 16    ) ERROR STOP 25
  IF ( ANY(G   .NE. Res ) ) ERROR STOP 26


  END
