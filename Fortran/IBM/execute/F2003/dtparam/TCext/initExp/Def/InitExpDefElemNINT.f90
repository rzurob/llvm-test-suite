! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefElemNINT.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  NINT
!*  (318902/318985/319559)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemNINT
  IMPLICIT NONE
  INTEGER :: I, J

  TYPE :: DT(N1,K1,K2,K3)    ! (20,4,8,16)
    INTEGER, KIND :: K1,K2,K3
    INTEGER, LEN  :: N1
    REAL(K1)      :: R4=2.4
    REAL(K2)      :: R8=2.5
    REAL(K3)      :: R6=2.6
  END TYPE

  TYPE (DT(20,4,8,16)), PARAMETER :: X(128) = DT(20,4,8,16)()

  INTEGER(KIND( NINT(A=X%R4, KIND=1)))            :: TI1(128) = NINT(A=X%R4, KIND=1)
  INTEGER(KIND( NINT(A=X%R8, KIND=KIND(X%R8)/4))) :: TI2(128) = NINT(A=X%R8, KIND=KIND(X%R8)/4)
  INTEGER(KIND( NINT(A=X%R4, KIND=KIND(X%R4))))   :: TI4(128) = NINT(A=X%R4, KIND=KIND(X%R4))
  INTEGER(KIND( NINT(A=X%R6, KIND=KIND(X%R6)/2))) :: TI8(128) = NINT(A=X%R6, KIND=KIND(X%R6)/2)

  INTEGER :: T1=MAXVAL(NINT(A=X%R4, KIND=1))
  INTEGER :: T2=MAXVAL(NINT(A=X%R8, KIND=KIND(X%R8)/4))
  INTEGER :: T3=MAXVAL(NINT(A=X%R4, KIND=KIND(X%R4)))
  INTEGER :: T4=MAXVAL(NINT(A=X%R6, KIND=KIND(X%R6)/2))

  IF ( KIND(TI1)  .NE.    1 )  ERROR STOP 11
  IF ( KIND(TI2)  .NE.    2 )  ERROR STOP 12
  IF ( KIND(TI4)  .NE.    4 )  ERROR STOP 14
  IF ( KIND(TI8)  .NE.    8 )  ERROR STOP 18

  IF (ANY( TI1  .NE. 2 )) ERROR STOP 21
  IF (ANY( TI2  .NE. 3 )) ERROR STOP 22
  IF (ANY( TI4  .NE. 2 )) ERROR STOP 23
  IF (ANY( TI8  .NE. 3 )) ERROR STOP 24

  IF ( T1   .NE. 2 )  ERROR STOP 31
  IF ( T2   .NE. 3 )  ERROR STOP 32
  IF ( T3   .NE. 2 )  ERROR STOP 33
  IF ( T4   .NE. 3 )  ERROR STOP 34


  END


