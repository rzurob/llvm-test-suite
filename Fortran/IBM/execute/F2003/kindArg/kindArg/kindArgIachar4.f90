!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar4
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*   
!*  IAchar is the inverse of the ACHAR function 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar4


  INTEGER(1) :: I1, II1(128)
  INTEGER(2) :: I2, II2(128)
  INTEGER(4) :: I4, II4(128)
  INTEGER(8) :: I8, II8(128)
     
  CHARACTER :: CC(128)

  CC=(/(ACHAR(I, KIND=1), I=0, 127)/)


  DO I1 = 0, 127
    IF (IACHAR(ACHAR(I1, KIND=KIND(1_1) ))        .NE. I1)                   STOP 11
    IF (IACHAR(ACHAR(I1, KIND=KIND(2_1) ))        .NE. I1)                   STOP 12
    IF (IACHAR(ACHAR(I1, KIND=KIND(4_1) ))        .NE. I1)                   STOP 13
    IF (IACHAR(ACHAR(I1, KIND=KIND(8_1) ))        .NE. I1)                   STOP 14
  END DO

  DO I2 = 0, 127
    IF (IACHAR(ACHAR(I2, KIND=KIND(1_1) ))        .NE. I2)                   STOP 21
    IF (IACHAR(ACHAR(I2, KIND=KIND(2_1) ))        .NE. I2)                   STOP 22
    IF (IACHAR(ACHAR(I2, KIND=KIND(4_1) ))        .NE. I2)                   STOP 23
    IF (IACHAR(ACHAR(I2, KIND=KIND(8_1) ))        .NE. I2)                   STOP 24
  END DO

  DO I4 = 0, 127
    IF (IACHAR(ACHAR(I4, KIND=KIND(1_1) ))        .NE. I4)                   STOP 41
    IF (IACHAR(ACHAR(I4, KIND=KIND(2_1) ))        .NE. I4)                   STOP 42
    IF (IACHAR(ACHAR(I4, KIND=KIND(4_1) ))        .NE. I4)                   STOP 43
    IF (IACHAR(ACHAR(I4, KIND=KIND(8_1) ))        .NE. I4)                   STOP 44
  END DO

  DO I8 = 0, 127
    IF (IACHAR(ACHAR(I8, KIND=KIND(1_1) ))        .NE. I8)                   STOP 81
    IF (IACHAR(ACHAR(I8, KIND=KIND(2_1) ))        .NE. I8)                   STOP 82
    IF (IACHAR(ACHAR(I8, KIND=KIND(4_1) ))        .NE. I8)                   STOP 83
    IF (IACHAR(ACHAR(I8, KIND=KIND(8_1) ))        .NE. I8)                   STOP 84
  END DO


  IF (ANY( IACHAR(C=(/(ACHAR(I1), I1=0,127)/), KIND=1_8) .NE. IACHAR(CC))) STOP 111 
  IF (ANY( IACHAR(C=(/(ACHAR(I2), I2=0,127)/), KIND=1_4) .NE. IACHAR(CC))) STOP 112 
  IF (ANY( IACHAR(C=(/(ACHAR(I4), I4=0,127)/), KIND=1_2) .NE. IACHAR(CC))) STOP 113 
  IF (ANY( IACHAR(C=(/(ACHAR(I8), I8=0,127)/), KIND=1_1) .NE. IACHAR(CC))) STOP 114 

  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/IACHAR(CC, KIND=1_1)/)))) .NE. (/IACHAR(CC)/))) STOP 115 
  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/IACHAR(CC, KIND=1_2)/)))) .NE. (/IACHAR(CC)/))) STOP 116 
  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/IACHAR(CC, KIND=1_4)/)))) .NE. (/IACHAR(CC)/))) STOP 117 
  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/IACHAR(CC, KIND=1_8)/)))) .NE. (/IACHAR(CC)/))) STOP 118 

  END

