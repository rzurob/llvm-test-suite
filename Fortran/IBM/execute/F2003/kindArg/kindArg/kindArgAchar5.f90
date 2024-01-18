!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgAchar5
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR 
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
!*  Entities with different attubute used for kind arg - allocatable/pointer
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar5


  INTEGER(1), ALLOCATABLE :: I1, II1(:), K1
  INTEGER(2), POINTER     :: I2, II2(:), K2
  INTEGER(4), ALLOCATABLE :: I4, II4(:), K4
  INTEGER(8), POINTER     :: I8, II8(:), K8
     
  !CHARACTER :: CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
 
  CHARACTER :: CC(0:127)
  CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(K1, SOURCE=1_1)
  ALLOCATE(K2, SOURCE=1_2)
  ALLOCATE(K4, SOURCE=1_4)
  ALLOCATE(K8, SOURCE=1_8)

  ALLOCATE(II1(128))
  ALLOCATE(II2(128))
  ALLOCATE(II4(128))
  ALLOCATE(II8(128))

  DO I1 = 0, 127
    IF (IACHAR(ACHAR(I1), KIND=K1%KIND )        .NE. I1)                   STOP 11
    IF (IACHAR(ACHAR(I1), KIND=K2%KIND )        .NE. I1)                   STOP 12
    IF (IACHAR(ACHAR(I1), KIND=K4%KIND )        .NE. I1)                   STOP 13
    IF (IACHAR(ACHAR(I1), KIND=K8%KIND )        .NE. I1)                   STOP 14
    IF (ACHAR(IACHAR(ACHAR(I1), KIND=K1%KIND ))   .NE. ACHAR(I1, KIND=K1%KIND ))    STOP 15
    IF (ACHAR(IACHAR(ACHAR(I1), KIND=K2%KIND-1 )) .NE. ACHAR(I1, KIND=K2%KIND-1 ))  STOP 16
    IF (ACHAR(IACHAR(ACHAR(I1), KIND=K4%KIND-3 )) .NE. ACHAR(I1, KIND=K4%KIND-3 ))  STOP 17
    IF (ACHAR(IACHAR(ACHAR(I1), KIND=K8%KIND-7 )) .NE. ACHAR(I1, KIND=K8%KIND-7 ))  STOP 18
  END DO

  DO I2 = 0, 127
    IF (IACHAR(ACHAR(I2, KIND=I1%KIND ))        .NE. I2)                   STOP 21
    IF (IACHAR(ACHAR(I2, KIND=II1%KIND ))       .NE. I2)                   STOP 22
    IF (IACHAR(ACHAR(I2, KIND=K1%KIND ))        .NE. I2)                   STOP 23
  END DO

  DO I4 = 0, 127
    IF (IACHAR(ACHAR(I4, KIND=K4%KIND-3 ))        .NE. I4)                   STOP 43
    IF (IACHAR(ACHAR(I4, KIND=K8%KIND-7 ))        .NE. I4)                   STOP 44
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=K4%KIND-3 ))) .NE. ACHAR(I4, KIND=K4%KIND-3 ))  STOP 45
    IF (ACHAR(IACHAR(ACHAR(I4, KIND=K8%KIND-7 ))) .NE. ACHAR(I4, KIND=K8%KIND-7 ))  STOP 48
  END DO


  IF (ANY( ACHAR(I=(/(I1, I1=0,127)/), KIND=K1%KIND)  .NE. CC)) STOP 111 
  IF (ANY( ACHAR(I=(/(I2, I2=0,127)/), KIND=I1%KIND)  .NE. CC)) STOP 112 
  IF (ANY( ACHAR(I=(/(I4, I4=0,127)/), KIND=II1%KIND) .NE. CC)) STOP 113 

  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/))) .NE. (/(I1, I1=0,127)/))) STOP 115 
  IF (ANY( IACHAR(ACHAR(I=(/(I2, I2=0,127)/))) .NE. (/(I2, I2=0,127)/))) STOP 116 
  IF (ANY( IACHAR(ACHAR(I=(/(I4, I4=0,127)/))) .NE. (/(I4, I4=0,127)/))) STOP 117 
  IF (ANY( IACHAR(ACHAR(I=(/(I8, I8=0,127)/))) .NE. (/(I8, I8=0,127)/))) STOP 118 


  END

