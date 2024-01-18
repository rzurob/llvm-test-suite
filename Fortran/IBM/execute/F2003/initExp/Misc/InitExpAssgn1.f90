!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn1.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Assignment on derived type
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    INTEGER(1) :: I1(128)=[(I,I=0,127)]
    INTEGER(2) :: I2(128)=[(I,I=0,127)]
    INTEGER(4) :: I4(128)=[(I,I=0,127)]
    INTEGER(8) :: I8(128)=[(I,I=0,127)]

    REAL(4)  :: R4(128)=[(I,I=0,127)]
    REAL(8)  :: R8(128)=[(I,I=0,127)]
    REAL(16) :: R6(128)=[(I,I=0,127)]

    COMPLEX(4) :: Z4(128)=[((I,-I),I=0,127)]
    COMPLEX(8) :: Z8(128)=[((I,-I),I=0,127)]
    COMPLEX(16):: Z6(128)=[((I,-I),I=0,127)]
  END TYPE

  END MODULE

  PROGRAM InitExpAssgn1
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT1
    TYPE(DT0) :: T1
  END TYPE

  TYPE :: DT
    TYPE(DT1) :: T
  END TYPE

  TYPE (DT) :: T(128)=[(DT(DT1(DT0())), I=0,127)]

  TYPE :: DTT1
    TYPE(DT0) :: T1(4)
  END TYPE

  TYPE :: DTT
    TYPE(DTT1) :: T(4)
  END TYPE

  TYPE (DTT) :: TT(4)=[(DTT(T=[(DTT1(T1=[(DT0(), K=0,3)]), J=0,3)]), I=0,3)]


  !TT= [(DTT(T=[(DT1(T1=[(DT0(), K=0,1)]), J=0,1)]), I=0,1)]

  DO I =1, 128
    IF ( ANY( T(I)%T%T1%I1  .NE. [(J,J=0,127)]  ) ) STOP 11
    IF ( ANY( T(I)%T%T1%I2  .NE. [(J,J=0,127)]  ) ) STOP 12
    IF ( ANY( T(I)%T%T1%I4  .NE. [(J,J=0,127)]  ) ) STOP 13
    IF ( ANY( T(I)%T%T1%I8  .NE. [(J,J=0,127)]  ) ) STOP 14

    IF ( ANY( T(I)%T%T1%R4  .NE. [(J,J=0,127)]  ) ) STOP 21
    IF ( ANY( T(I)%T%T1%R8  .NE. [(J,J=0,127)]  ) ) STOP 22
    IF ( ANY( T(I)%T%T1%R6  .NE. [(J,J=0,127)]  ) ) STOP 23

    IF ( ANY( T(I)%T%T1%Z4  .NE. [((J,-J),J=0,127)]  ) ) STOP 31
    IF ( ANY( T(I)%T%T1%Z8  .NE. [((J,-J),J=0,127)]  ) ) STOP 32
    IF ( ANY( T(I)%T%T1%Z6  .NE. [((J,-J),J=0,127)]  ) ) STOP 33
  END DO

  DO I =1, 4
  DO J =1, 4
  DO K =1, 4
    IF ( ANY( TT(I)%T(J)%T1(K)%I1  .NE. [(J,J=0,127)]  ) ) STOP 41
    IF ( ANY( TT(I)%T(J)%T1(K)%I2  .NE. [(J,J=0,127)]  ) ) STOP 42
    IF ( ANY( TT(I)%T(J)%T1(K)%I4  .NE. [(J,J=0,127)]  ) ) STOP 43
    IF ( ANY( TT(I)%T(J)%T1(K)%I8  .NE. [(J,J=0,127)]  ) ) STOP 44

    IF ( ANY( TT(I)%T(J)%T1(K)%R4  .NE. [(J,J=0,127)]  ) ) STOP 51
    IF ( ANY( TT(I)%T(J)%T1(K)%R8  .NE. [(J,J=0,127)]  ) ) STOP 52
    IF ( ANY( TT(I)%T(J)%T1(K)%R6  .NE. [(J,J=0,127)]  ) ) STOP 53

    IF ( ANY( TT(I)%T(J)%T1(K)%Z4  .NE. [((J,-J),J=0,127)]  ) ) STOP 61
    IF ( ANY( TT(I)%T(J)%T1(K)%Z8  .NE. [((J,-J),J=0,127)]  ) ) STOP 62
    IF ( ANY( TT(I)%T(J)%T1(K)%Z6  .NE. [((J,-J),J=0,127)]  ) ) STOP 63
  END DO
  END DO
  END DO

  END



