!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgAchar8
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
!*  Achar called in arr constructor 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar8
  IMPLICIT NONE


  INTEGER(1), ALLOCATABLE :: II1(:)
  INTEGER(4), ALLOCATABLE :: I4
  INTEGER(8), POINTER     :: I8
  
  INTEGER                 :: I
   
! CHARACTER :: CC(0:127)=(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  CHARACTER :: CC(0:127)
  CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
 
  ALLOCATE(I4)
  ALLOCATE(I8)


  ALLOCATE(II1(128))

  DO I4 = 0, 127
    IF (ANY((/ACHAR(I4, KIND=IACHAR(ACHAR(1, KIND=1_1), KIND=8))/)  .NE. CC(I4:I4)) ) STOP 11
    IF (ANY((/ACHAR(I4, KIND=IACHAR(ACHAR(1, KIND=1_2), KIND=4))/)  .NE. CC(I4:I4)) ) STOP 12
    IF (ANY((/ACHAR(I4, KIND=IACHAR(ACHAR(1, KIND=1_4), KIND=2))/)  .NE. CC(I4:I4)) ) STOP 13
    IF (ANY((/ACHAR(I4, KIND=IACHAR(ACHAR(1, KIND=1_8), KIND=1))/)  .NE. CC(I4:I4)) ) STOP 14
  END DO

  DO I8 = 0, 127
    IF (ANY((/ACHAR(I8, KIND=KIND(IACHAR(ACHAR(I8), KIND=1_8)))/)  .NE. CC(I8:I8)) ) STOP 21
    IF (ANY((/ACHAR(I8, KIND=KIND(IACHAR(ACHAR(I8), KIND=1_4)))/)  .NE. CC(I8:I8)) ) STOP 22
    IF (ANY((/ACHAR(I8, KIND=KIND(IACHAR(ACHAR(I8), KIND=1_2)))/)  .NE. CC(I8:I8)) ) STOP 23
    IF (ANY((/ACHAR(I8, KIND=KIND(IACHAR(ACHAR(I8), KIND=1_1)))/)  .NE. CC(I8:I8)) ) STOP 24
  END DO


  II1=(/(I, I=0,127)/)
  IF (ANY((/ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1_1, KIND=1_1), KIND=8_1))/)  .NE. CC )) STOP 111
  IF (ANY((/ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1_2, KIND=1_2), KIND=4_4))/)  .NE. CC )) STOP 112
  IF (ANY((/ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1_4, KIND=1_4), KIND=2_8))/)  .NE. CC )) STOP 113
  IF (ANY((/ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1_8, KIND=1_8), KIND=1_2))/)  .NE. CC )) STOP 114


  END

