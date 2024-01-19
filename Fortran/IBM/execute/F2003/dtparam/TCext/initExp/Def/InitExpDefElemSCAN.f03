! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/initExp/Def/InitExpDefElemSCAN.f
! opt variations: -qnock

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
!*  -  SCAN
!*  (325139)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSCAN
  IMPLICIT NONE
  INTEGER :: I, J


  TYPE :: DT(K1,N1)    ! (1,32)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C= "  012345  "
  END TYPE

  TYPE (DT(1,32)), PARAMETER :: X(4,4) = DT(1,32)()
  REAL(4), PARAMETER :: RR=12.

  INTEGER(KIND(SCAN(STRING=X%C, SET=X%C,           KIND=1))) :: TI1(4,4)=SCAN(STRING=X%C,      SET=X%C,      KIND=1)
  INTEGER(KIND(SCAN(STRING=X%C, SET=X%C(1:0),      KIND=2))) :: TI2(4,4)=SCAN(STRING=X%C,      SET=X%C(1:0), KIND=2)
  INTEGER(KIND(SCAN(STRING=X%C(1:2), SET=X%C,      KIND=4))) :: TI4(4,4)=SCAN(STRING=X%C(2:1), SET=X%C,      KIND=4)
  INTEGER(KIND(SCAN(STRING=X%C(2:1), SET=X%C(1:0), KIND=8))) :: TI8(4,4)=SCAN(STRING=X%C(1:2), SET=X%C(2:1), KIND=8)


  IF ( KIND(TI1)  .NE.    1 )  ERROR STOP 11
  IF ( KIND(TI2)  .NE.    2 )  ERROR STOP 12
  IF ( KIND(TI4)  .NE.    4 )  ERROR STOP 14
  IF ( KIND(TI8)  .NE.    8 )  ERROR STOP 18

  IF (ANY( TI1  .NE. 1   )) ERROR STOP 21
  IF (ANY( TI2  .NE. 0   )) ERROR STOP 22
  IF (ANY( TI4  .NE. 0   )) ERROR STOP 24
  IF (ANY( TI8  .NE. 0   )) ERROR STOP 28


  END


