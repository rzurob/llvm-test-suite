! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/initExp/Def/InitExpDefElemVERIFY.f
! opt variations: -qnock -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 14, 2006
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
!*  -  VERIFY
!*  (not support kind for verify yet)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemVERIFY
  IMPLICIT NONE
  INTEGER :: I, J


  TYPE :: DT(K1,N1,K2,N2)    ! (1,38,1,16)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1,N2
    CHARACTER(kind=K1,len=N1) :: C = "1234567890abcdefghijklmnopqrstuvwxyz  "
    CHARACTER(kind=K2,len=N2) :: C1= "543210  abc ABC "
  END TYPE

  TYPE (DT(1,38,1,16)), PARAMETER :: X(4,4) = DT(1,38,1,16)()
  REAL(4), PARAMETER :: RR=12.

  INTEGER(KIND(VERIFY(STRING=X%C,      SET=X%C1,      KIND=1))) :: TI1(4,4)=VERIFY(STRING=X%C,      SET=X%C1,      KIND=1)
  INTEGER(KIND(VERIFY(STRING=X%C,      SET=X%C1(2:1), KIND=2))) :: TI2(4,4)=VERIFY(STRING=X%C,      SET=X%C1(1:2), KIND=2)
  INTEGER(KIND(VERIFY(STRING=X%C(1:2), SET=X%C1,      KIND=4))) :: TI4(4,4)=VERIFY(STRING=X%C(2:1), SET=X%C1,      KIND=4)
  INTEGER(KIND(VERIFY(STRING=X%C(2:1), SET=X%C1(2:1), KIND=8))) :: TI8(4,4)=VERIFY(STRING=X%C(1:2), SET=X%C1(2:1), KIND=8)

  INTEGER  :: TX1(4,4) = VERIFY(STRING=X%C, SET=X%C1(1:6), BACK=.TRUE.)
  INTEGER  :: TX2(4,4) = VERIFY(STRING=X%C, SET=X%C1( : ), BACK=.TRUE.)


  IF ( KIND(TI1)  .NE.    1 )  ERROR STOP 11
  IF ( KIND(TI2)  .NE.    2 )  ERROR STOP 12
  IF ( KIND(TI4)  .NE.    4 )  ERROR STOP 14
  IF ( KIND(TI8)  .NE.    8 )  ERROR STOP 18

  IF (ANY( TI1  .NE. 6   )) ERROR STOP 21
  IF (ANY( TI2  .NE. 1   )) ERROR STOP 22
  IF (ANY( TI4  .NE. 0   )) ERROR STOP 24
  IF (ANY( TI8  .NE. 1   )) ERROR STOP 25
  IF (ANY( TX1  .NE. 38  )) ERROR STOP 26
  IF (ANY( TX2  .NE. 36  )) ERROR STOP 27



  END


