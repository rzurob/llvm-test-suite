!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Achar is the inverse of the IACHAR function
!*
!*  (322503)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar4

  INTEGER    :: I
  INTEGER(1) :: I1, II1(128)
  INTEGER(2) :: I2, II2(128)
  INTEGER(4) :: I4, II4(128)
  INTEGER(8) :: I8, II8(128)

  CHARACTER :: CC(0:127)=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
  !CHARACTER :: CC(0:127)
  !CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)


  DO I1 = 0, 127
    IF (IACHAR(ACHAR(I1, KIND=1_1 ))        .NE. I1)                   ERROR STOP 11
    IF (IACHAR(ACHAR(I1, KIND=1_2 ))        .NE. I1)                   ERROR STOP 12
    IF (IACHAR(ACHAR(I1, KIND=1_4 ))        .NE. I1)                   ERROR STOP 13
    IF (IACHAR(ACHAR(I1, KIND=1_8 ))        .NE. I1)                   ERROR STOP 14
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=1_1 ))) .NE. ACHAR(I1, KIND=1_1 )) ERROR STOP 15
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=1_2 ))) .NE. ACHAR(I1, KIND=1_2 )) ERROR STOP 16
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=1_4 ))) .NE. ACHAR(I1, KIND=1_4 )) ERROR STOP 17
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=1_8 ))) .NE. ACHAR(I1, KIND=1_8 )) ERROR STOP 18
  END DO

  DO I2 = 0, 127
    IF (IACHAR(ACHAR(I2, KIND=1_1 ))        .NE. I2)                   ERROR STOP 21
    IF (IACHAR(ACHAR(I2, KIND=1_2 ))        .NE. I2)                   ERROR STOP 22
    IF (IACHAR(ACHAR(I2, KIND=1_4 ))        .NE. I2)                   ERROR STOP 23
    IF (IACHAR(ACHAR(I2, KIND=1_8 ))        .NE. I2)                   ERROR STOP 24
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=1_1 ))) .NE. ACHAR(I2, KIND=1_1 )) ERROR STOP 25
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=1_2 ))) .NE. ACHAR(I2, KIND=1_2 )) ERROR STOP 26
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=1_4 ))) .NE. ACHAR(I2, KIND=1_4 )) ERROR STOP 27
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=1_8 ))) .NE. ACHAR(I2, KIND=1_8 )) ERROR STOP 28
  END DO

  DO I4 = 0, 127
    IF (IACHAR(ACHAR(I4, KIND=1_1 ))        .NE. I4)                   ERROR STOP 41
    IF (IACHAR(ACHAR(I4, KIND=1_2 ))        .NE. I4)                   ERROR STOP 42
    IF (IACHAR(ACHAR(I4, KIND=1_4 ))        .NE. I4)                   ERROR STOP 43
    IF (IACHAR(ACHAR(I4, KIND=1_8 ))        .NE. I4)                   ERROR STOP 44
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=1_1 ))) .NE. ACHAR(I4, KIND=1_1 )) ERROR STOP 45
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=1_2 ))) .NE. ACHAR(I4, KIND=1_2 )) ERROR STOP 46
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=1_4 ))) .NE. ACHAR(I4, KIND=1_4 )) ERROR STOP 47
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=1_8 ))) .NE. ACHAR(I4, KIND=1_8 )) ERROR STOP 48
  END DO

  DO I8 = 0, 127
    IF (IACHAR(ACHAR(I8, KIND=1_1 ))        .NE. I8)                   ERROR STOP 81
    IF (IACHAR(ACHAR(I8, KIND=1_2 ))        .NE. I8)                   ERROR STOP 82
    IF (IACHAR(ACHAR(I8, KIND=1_4 ))        .NE. I8)                   ERROR STOP 83
    IF (IACHAR(ACHAR(I8, KIND=1_8 ))        .NE. I8)                   ERROR STOP 84
    IF (ACHAR(IACHAR(ACHAR(I8, KIND=1_1 ))) .NE. ACHAR(I8, KIND=1_1 )) ERROR STOP 85
    IF (ACHAR(IACHAR(ACHAR(I8, KIND=1_2 ))) .NE. ACHAR(I8, KIND=1_2 )) ERROR STOP 86
    IF (ACHAR(IACHAR(ACHAR(I8, KIND=1_4 ))) .NE. ACHAR(I8, KIND=1_4 )) ERROR STOP 87
    IF (ACHAR(IACHAR(ACHAR(I8, KIND=1_8 ))) .NE. ACHAR(I8, KIND=1_8 )) ERROR STOP 88
  END DO



  IF (ANY( ACHAR(I=(/(I1, I1=0,127)/), KIND=1_8) .NE. CC)) ERROR STOP 111
  IF (ANY( ACHAR(I=(/(I2, I2=0,127)/), KIND=1_4) .NE. CC)) ERROR STOP 112
  IF (ANY( ACHAR(I=(/(I4, I4=0,127)/), KIND=1_2) .NE. CC)) ERROR STOP 113
  IF (ANY( ACHAR(I=(/(I8, I8=0,127)/), KIND=1_1) .NE. CC)) ERROR STOP 114

  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=1_8)) .NE. (/(I1, I1=0,127)/))) ERROR STOP 115
  IF (ANY( IACHAR(ACHAR(I=(/(I2, I2=0,127)/), KIND=1_4)) .NE. (/(I2, I2=0,127)/))) ERROR STOP 116
  IF (ANY( IACHAR(ACHAR(I=(/(I4, I4=0,127)/), KIND=1_2)) .NE. (/(I4, I4=0,127)/))) ERROR STOP 117
  IF (ANY( IACHAR(ACHAR(I=(/(I8, I8=0,127)/), KIND=1_1)) .NE. (/(I8, I8=0,127)/))) ERROR STOP 118


  END

