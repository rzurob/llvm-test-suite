!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : COUNT
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
!*  Count the number of true elements of MASK along dimension DIM
!*
!*  (322520)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgCount3
  IMPLICIT NONE

  INTEGER(1) :: I1
  LOGICAL(1) :: L1(4,4)
  INTEGER(2) :: I2
  LOGICAL(2) :: L2(4,4)
  INTEGER(4) :: I4
  LOGICAL(4) :: L4(4,4)
  INTEGER(8) :: I8
  LOGICAL(8) :: L8(4,4)

  L1= .TRUE.;L1(1,1)=.FALSE.
  L2= .TRUE.;L2(2,2)=.FALSE.
  L4= .TRUE.;L4(3,3)=.FALSE.
  L8= .TRUE.;L8(4,4)=.FALSE.


  DO I1 =1, 2
    IF (ANY(COUNT(L1, KIND=1_1, DIM=I1 ) .NE. (/3,4,4,4/) )) STOP 11
    IF (ANY(COUNT(L1, KIND=1_2, DIM=I1 ) .NE. (/3,4,4,4/) )) STOP 12
    IF (ANY(COUNT(L1, KIND=1_4, DIM=I1 ) .NE. (/3,4,4,4/) )) STOP 13
    IF (ANY(COUNT(L1, KIND=1_8, DIM=I1 ) .NE. (/3,4,4,4/) )) STOP 14
  END DO

  DO I2 =1, 2
    IF (ANY(COUNT(L2, DIM=I2, KIND=1_1 ) .NE. (/4,3,4,4/) )) STOP 21
    IF (ANY(COUNT(L2, DIM=I2, KIND=1_2 ) .NE. (/4,3,4,4/) )) STOP 22
    IF (ANY(COUNT(L2, DIM=I2, KIND=1_4 ) .NE. (/4,3,4,4/) )) STOP 23
    IF (ANY(COUNT(L2, DIM=I2, KIND=1_8 ) .NE. (/4,3,4,4/) )) STOP 24
  END DO

  DO I4 =1, 2
    IF (ANY(COUNT(DIM=I4, MASK=L4, KIND=1_1 ) .NE. (/4,4,3,4/) )) STOP 31
    IF (ANY(COUNT(DIM=I4, MASK=L4, KIND=1_2 ) .NE. (/4,4,3,4/) )) STOP 32
    IF (ANY(COUNT(DIM=I4, MASK=L4, KIND=1_4 ) .NE. (/4,4,3,4/) )) STOP 33
    IF (ANY(COUNT(DIM=I4, MASK=L4, KIND=1_8 ) .NE. (/4,4,3,4/) )) STOP 34
  END DO

  DO I8 = 1, 2
    IF (ANY(COUNT(L8, I8,  KIND=1_1 ) .NE. (/4,4,4,3/) )) STOP 41
    IF (ANY(COUNT(L8, I8,  KIND=1_2 ) .NE. (/4,4,4,3/) )) STOP 42
    IF (ANY(COUNT(L8, I8,  KIND=1_4 ) .NE. (/4,4,4,3/) )) STOP 43
    IF (ANY(COUNT(L8, I8,  KIND=1_8 ) .NE. (/4,4,4,3/) )) STOP 44
  END DO


  END

