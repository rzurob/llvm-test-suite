!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemVERIFY.f
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


  TYPE :: DT
    CHARACTER(38) :: C = "1234567890abcdefghijklmnopqrstuvwxyz  "
    CHARACTER(16) :: C1= "543210  abc ABC "
  END TYPE

  TYPE (DT), PARAMETER :: X(4,4) = DT()
  REAL(4), PARAMETER :: RR=12.

  INTEGER(KIND(VERIFY(STRING=X%C,      SET=X%C1,      KIND=1))) :: TI1(4,4)=VERIFY(STRING=X%C,      SET=X%C1,      KIND=1)
  INTEGER(KIND(VERIFY(STRING=X%C,      SET=X%C1(2:1), KIND=2))) :: TI2(4,4)=VERIFY(STRING=X%C,      SET=X%C1(1:2), KIND=2)
  INTEGER(KIND(VERIFY(STRING=X%C(1:2), SET=X%C1,      KIND=4))) :: TI4(4,4)=VERIFY(STRING=X%C(2:1), SET=X%C1,      KIND=4)
  INTEGER(KIND(VERIFY(STRING=X%C(2:1), SET=X%C1(2:1), KIND=8))) :: TI8(4,4)=VERIFY(STRING=X%C(1:2), SET=X%C1(2:1), KIND=8)

  INTEGER  :: TX1(4,4) = VERIFY(STRING=X%C, SET=X%C1(1:6), BACK=.TRUE.)
  INTEGER  :: TX2(4,4) = VERIFY(STRING=X%C, SET=X%C1( : ), BACK=.TRUE.)


  IF ( KIND(TI1)  .NE.    1 )  STOP 11
  IF ( KIND(TI2)  .NE.    2 )  STOP 12
  IF ( KIND(TI4)  .NE.    4 )  STOP 14
  IF ( KIND(TI8)  .NE.    8 )  STOP 18

  IF (ANY( TI1  .NE. 6   )) STOP 21
  IF (ANY( TI2  .NE. 1   )) STOP 22
  IF (ANY( TI4  .NE. 0   )) STOP 24
  IF (ANY( TI8  .NE. 1   )) STOP 25
  IF (ANY( TX1  .NE. 38  )) STOP 26
  IF (ANY( TX2  .NE. 36  )) STOP 27



  END


