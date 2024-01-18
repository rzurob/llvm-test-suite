!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgSize2
!*
!*  DATE                       : Jun. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE
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


  PROGRAM kindArgSize2

  LOGICAL        :: ARRAY(3) = (/.TRUE., .TRUE., .TRUE./)
  INTEGER(1),    PARAMETER :: KIND(3) = (/1,1,1/)

  PRINT*, SIZE(Array, KINDD=I(1), DIM=1)
  PRINT*, SIZE(Array, KIn=kind(1))
  PRINT*, SIZE(dim=1, KIND=1)
  PRINT*, SIZE(KIND=kind(1), ARRAY=Array, KIND=1_2)

  PRINT*, SIZE(kind /= kind, kInD=SIZE((/1/))*kind(1)) ! this is fine
  PRINT*, SIZE(DIM=Kind(3),ARRAY=Array, kInD=Array(3)%Kind) ! this is fine

  END

