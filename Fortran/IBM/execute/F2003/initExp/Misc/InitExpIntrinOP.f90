!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29 2006
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
!*  Characteristics of intrinsic op on entities of zero length
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntrinOP

  INTEGER(KIND=1)  :: I1(128)=KIND(1_1 + 1_1 + 0_4)
  INTEGER(KIND=2)  :: I2(128)=KIND(0_1 + 1_2 + 1_2)
  INTEGER(KIND=4)  :: I4(128)=KIND(1_1 + 0_4 + 1_4)
  INTEGER(KIND=8)  :: I8(128)=KIND(0_4 + 1_8 + 0_4)

  INTEGER(KIND=1)  :: L1(128)=KIND(.TRUE._1  .OR.  .FALSE._2)
  INTEGER(KIND=2)  :: L2(128)=KIND(.TRUE._1  .AND. .TRUE._1  .OR. .FALSE._2)
  INTEGER(KIND=4)  :: L4(128)=KIND(.TRUE._1  .OR.  .FALSE._2 .OR. .TRUE._4)
  INTEGER(KIND=8)  :: L8(128)=KIND(.FALSE._1 .OR.  .FALSE._2 .OR. .TRUE._8)

  INTEGER :: R4(128)= KIND(1_1 + 1_1 + 0._4)
  INTEGER :: R8(128)= KIND(0._8 + 1_4 + 1._4)
  INTEGER :: R6(128)= KIND(0._16 + 1_8 + 0._4)

  INTEGER :: Z4(128)= KIND((0._4,0._4)  + 0._8)
  INTEGER :: Z8(128)= KIND(0._8 + (0._4,0._4))
  INTEGER :: Z6(128)= KIND((0._4, 0_4) + (0._4, 0._4) + 0._16 )



  IF ( ANY(I1   .NE. 4 ) ) STOP 11
  IF ( ANY(I2   .NE. 2 ) ) STOP 12
  IF ( ANY(I4   .NE. 4 ) ) STOP 13
  IF ( ANY(I8   .NE. 8 ) ) STOP 14

  IF ( ANY(L1   .NE. 2 ) ) STOP 21
  IF ( ANY(L2   .NE. 2 ) ) STOP 22
  IF ( ANY(L4   .NE. 4 ) ) STOP 23
  IF ( ANY(L8   .NE. 8 ) ) STOP 24

  IF ( ANY(R4   .NE. 4 ) ) STOP 31
  IF ( ANY(R8   .NE. 8 ) ) STOP 32
  IF ( ANY(R6   .NE. 16) ) STOP 33

  IF ( ANY(Z4   .NE. 8 ) ) STOP 41
  IF ( ANY(Z8   .NE. 8 ) ) STOP 42
  IF ( ANY(Z6   .NE. 16) ) STOP 43


  END



