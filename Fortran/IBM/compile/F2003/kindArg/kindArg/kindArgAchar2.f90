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
!*  characteristics :: the keyword - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/)
  INTEGER(1),    PARAMETER :: KIND(3) = (/1,1,1/)

  PRINT*, ACHAR(1_1, KINDD=I(1))
  PRINT*, ACHAR(I=1_1, KID=kind(1))
  PRINT*, ACHAR(KIND=I(1), KIND=2)
  PRINT*, ACHAR(I=(/10,11/), KIND=1, KIND=1)
  PRINT*, ACHAR(KIND=1_2, I=(/10_2,11_2/), KIND=1_2)

  PRINT*, ACHAR(i=IACHAR("A"), kInD=I(2)) ! this is fine
  PRINT*, ACHAR(i=KIND, kInD=KIND(3)) ! this is fine

  END

