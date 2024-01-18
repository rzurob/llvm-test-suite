!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar6
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 16, 2006
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
!*  Entities with different attubute used for kind arg - associate/select type 
!*    
!*  (322269) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar6
  IMPLICIT NONE

  INTEGER :: I

  INTEGER(1), ALLOCATABLE :: I01, II01(:), K01
  INTEGER(2), POINTER     :: I02, II02(:), K02
  CLASS(*),   ALLOCATABLE :: I04, II04(:), K04
  CLASS(*),   POINTER     :: I08, II08(:), K08
     
  !CHARACTER :: CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
  CHARACTER :: CC(128)

  CC = (/(ACHAR(I=I, KIND=1), I=0, 127)/)

  ALLOCATE(I01)
  ALLOCATE(I02)
  ALLOCATE(INTEGER(4)::I04)
  ALLOCATE(INTEGER(8)::I08)

  ALLOCATE(K01, SOURCE=1_1)
  ALLOCATE(K02, SOURCE=1_2)
  ALLOCATE(K04, SOURCE=1_4)
  ALLOCATE(K08, SOURCE=1_8)

  ALLOCATE(II01(128))
  ALLOCATE(II02(128))
  ALLOCATE(INTEGER(4)::II04(128))
  ALLOCATE(INTEGER(8)::II08(128))


  ASSOCIATE (I1=>I01,II1=>II01,K1=>K01, I2=>I02,II2=>II02,K2=>K02)
  SELECT TYPE (I4 => I04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (II4 => II04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (K4 => K04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (I8 => I08)
  TYPE IS (INTEGER(8))
  SELECT TYPE (II8 => II08)
  TYPE IS (INTEGER(8))
  SELECT TYPE (K8 => K08)
  TYPE IS (INTEGER(8))


 
  DO I1 = 0, 127
    IF (IACHAR(ACHAR(I1), KIND=KIND((/K1/))  )     .NE. I1)                   STOP 11
    IF (IACHAR(ACHAR(I1), KIND=KIND((/K2/))-1)     .NE. I1)                   STOP 12
    IF (IACHAR(ACHAR(I1), KIND=KIND((/K4/))-3)     .NE. I1)                   STOP 13
    IF (IACHAR(ACHAR(I1), KIND=KIND((/K8/))-7)     .NE. I1)                   STOP 14
  END DO

  DO I2 = 0, 127
    IF (IACHAR(ACHAR(I2), KIND=KIND((/K1/))  )     .NE. I2)                   STOP 21
    IF (IACHAR(ACHAR(I2), KIND=KIND((/K2/))-1)     .NE. I2)                   STOP 22
    IF (IACHAR(ACHAR(I2), KIND=KIND((/K4/))-3)     .NE. I2)                   STOP 23
    IF (IACHAR(ACHAR(I2), KIND=KIND((/K8/))-7)     .NE. I2)                   STOP 24
  END DO

  DO I4 = 0, 127
    IF (IACHAR(ACHAR(I4), KIND=KIND((/K1/))  )     .NE. I4)                   STOP 41
    IF (IACHAR(ACHAR(I4), KIND=KIND((/K2/))-1)     .NE. I4)                   STOP 42
    IF (IACHAR(ACHAR(I4), KIND=KIND((/K4/))-3)     .NE. I4)                   STOP 43
    IF (IACHAR(ACHAR(I4), KIND=KIND((/K8/))-7)     .NE. I4)                   STOP 44
  END DO

  DO I8 = 0, 127
    IF (IACHAR(ACHAR(I8), KIND=KIND((/K1/))  )     .NE. I8)                   STOP 81
    IF (IACHAR(ACHAR(I8), KIND=KIND((/K2/))-1)     .NE. I8)                   STOP 82
    IF (IACHAR(ACHAR(I8), KIND=KIND((/K4/))-3)     .NE. I8)                   STOP 83
    IF (IACHAR(ACHAR(I8), KIND=KIND((/K8/))-7)     .NE. I8)                   STOP 84
  END DO


  II1=(/(I,I=0,127)/)
  II2 = II1
  II4 = II2
  II8 = II4

  IF (ANY( IACHAR(C=CC, KIND=II1%KIND  ) .NE. II8)) STOP 111 
  IF (ANY( IACHAR(C=CC, KIND=II2%KIND-1) .NE. II4)) STOP 112 
  IF (ANY( IACHAR(C=CC, KIND=II4%KIND-3) .NE. II2)) STOP 113 
  IF (ANY( IACHAR(C=CC, KIND=II8%KIND-7) .NE. II1)) STOP 114 


  CLASS DEFAULT
    STOP 91
  END SELECT 
  CLASS DEFAULT
    STOP 92
  END SELECT 
  CLASS DEFAULT
    STOP 93
  END SELECT 
  CLASS DEFAULT
    STOP 94
  END SELECT 
  CLASS DEFAULT
    STOP 95
  END SELECT 
  CLASS DEFAULT
    STOP 96
  END SELECT 

  END ASSOCIATE

  END

