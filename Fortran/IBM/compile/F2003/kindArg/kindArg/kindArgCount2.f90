!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount2
!*
!*  DATE                       : Jun. 14, 2006
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
!*  characteristics :: the keyword - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgCount2

  LOGICAL,       PARAMETER :: Mask(3) = (/.TRUE., .TRUE., .TRUE./)
  INTEGER(1),    PARAMETER :: KIND(3) = (/1,1,1/)

  PRINT*, COUNT(Mask, KINDD=I(1), DIM=1)
  PRINT*, COUNT(MAsk, KIn=kind(1))
  PRINT*, COUNT(dim=1, KIND=1)
  PRINT*, COUNT(KIND=kind(1), MASK=Mask, KIND=1_2)

  PRINT*, COUNT(kind /= kind, kInD=SIZE((/1/))*kind(1)) ! this is fine
  PRINT*, COUNT(DIM=Kind(3),MASK=Mask, kInD=KIND(3)) ! this is fine

  END

