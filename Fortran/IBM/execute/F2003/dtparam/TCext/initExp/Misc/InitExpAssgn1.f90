! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Misc/InitExpAssgn1.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

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

  TYPE :: DT0(N1,K1,K2,K3,K4,K5)    ! (20,1,2,4,8,16)
    INTEGER, KIND :: K1,K2,K3,K4,K5
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I1(128)=[(I,I=0,127)]
    INTEGER(K2)   :: I2(128)=[(I,I=0,127)]
    INTEGER(K3)   :: I4(128)=[(I,I=0,127)]
    INTEGER(K4)   :: I8(128)=[(I,I=0,127)]

    REAL(K3)      :: R4(128)=[(I,I=0,127)]
    REAL(K4)      :: R8(128)=[(I,I=0,127)]
    REAL(K5)      :: R6(128)=[(I,I=0,127)]

    COMPLEX(K3)   :: Z4(128)=[((I,-I),I=0,127)]
    COMPLEX(K4)   :: Z8(128)=[((I,-I),I=0,127)]
    COMPLEX(K5)   :: Z6(128)=[((I,-I),I=0,127)]
  END TYPE

  END MODULE

  PROGRAM InitExpAssgn1
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT1(K6,N2,K7,K8,K9,K10)    ! (4,20,1,2,8,16)
    INTEGER, KIND                 :: K6,K7,K8,K9,K10
    INTEGER, LEN                  :: N2
    TYPE(DT0(N2,K7,K8,K6,K9,K10)) :: T1
  END TYPE

  TYPE :: DT(K11,N3,K12,K13,K14,K15)    ! (4,20,1,2,8,16)
    INTEGER, KIND                     :: K11,K12,K13,K14,K15
    INTEGER, LEN                      :: N3
    TYPE(DT1(K11,N3,K12,K13,K14,K15)) :: T
  END TYPE

  TYPE (DT(4,20,1,2,8,16)) :: T(128)=[(DT(4,20,1,2,8,16)(DT1(4,20,1,2,8,16)(DT0(20,1,2,4,8,16)())), I=0,127)]

  TYPE :: DTT1(K16,N4,K17,K18,K19,K20)    ! (4,20,1,2,8,16)
    INTEGER, KIND                     :: K16,K17,K18,K19,K20
    INTEGER, LEN                      :: N4
    TYPE(DT0(N4,K17,K18,K16,K19,K20)) :: T1(4)
  END TYPE

  TYPE :: DTT(K21,N5,K22,K23,K24,K25)    ! (4,20,1,2,8,16)
    INTEGER, KIND                      :: K21,K22,K23,K24,K25
    INTEGER, LEN                       :: N5
    TYPE(DTT1(K21,N5,K22,K23,K24,K25)) :: T(4)
  END TYPE

  TYPE (DTT(4,20,1,2,8,16)) :: TT(4)=[(DTT(4,20,1,2,8,16)(T=[(DTT1(4,20,1,2,8,16)(T1=[(DT0(20,1,2,4,8,16)(), K=0,3)]), J=0,3)]), I=0,3)]


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



