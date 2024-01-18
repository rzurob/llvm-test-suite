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
!*  type-spec in allocate stmt
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpeAllocate1

  IMPLICIT REAL(KIND=4_8)  (A)
  IMPLICIT REAL(KIND=8_4)  (B)
  IMPLICIT REAL(KIND=16_2) (C)

  IMPLICIT COMPLEX(KIND=KIND((/0.E0/))) (E)
  IMPLICIT COMPLEX(KIND=KIND((/0.D0/))) (F)
  IMPLICIT COMPLEX(KIND=KIND((/0.Q0/))) (G)

  INTEGER   :: I
  REAL(4)   :: R4
  REAL(8)   :: R8
  REAL(16)  :: R6


  REAL, PARAMETER :: Res(128)=(/(-I, I=0, 127)/)

  ALLOCATABLE ::  A(:)
  ALLOCATABLE ::  B(:)
  ALLOCATABLE ::  C(:)

  ALLOCATABLE ::  E(:)
  ALLOCATABLE ::  F(:)
  ALLOCATABLE ::  G(:)

  ALLOCATE(A(128), SOURCE=[REAL(KIND=A%KIND) :: (-R4, R4=0, 127, 1.0)] )
  ALLOCATE(B(128), SOURCE=[REAL(KIND=B%KIND) :: (-R8, R8=0, 127), 1.0] )
  ALLOCATE(C(128), SOURCE=[REAL(KIND=C%KIND) :: (-R6, R6=0, 127), 1.0] )

  ALLOCATE(E(128), SOURCE=[COMPLEX(KIND=E%KIND) :: ((-R4, 0.), R4=0, 127, 1.0)] )
  ALLOCATE(F(128), SOURCE=[COMPLEX(KIND=F%KIND) :: ((-R8, 0._8), R8=0, 127, 1.0)] )
  ALLOCATE(G(128), SOURCE=[COMPLEX(KIND=G%KIND) :: ((-R6, 0._16), R6=0, 127, 1.0)] )

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
