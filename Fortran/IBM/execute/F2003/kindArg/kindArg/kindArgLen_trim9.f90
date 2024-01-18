!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim9
!*
!*  DATE                       : Jun. 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM
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
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim9
  IMPLICIT CHARACTER(129)(C)

  DO I=1, 128
    CC(I:I)="X"
  END DO
  CC(129:129)=" "

  DO I = 1, 128
    IF (     LEN_TRIM(STRING=CC(I:129), KIND=KIND(J))    .NE. 129-I )   STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I:129), KIND=KIND(J)))   .NE. KIND(J) ) STOP 12
  END DO

  END

