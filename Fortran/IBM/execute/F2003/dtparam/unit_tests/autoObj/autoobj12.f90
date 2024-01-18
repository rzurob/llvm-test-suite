!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj12
!*
!*  DATE                       : Nov. 30, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The length parameter depends on a V in common block
!*  the component is character
!*
!*  ()
!*
!234567891223456789122345678912234567891223456789122345678912234567890

  PROGRAM autoobj12
  INTEGER N
  COMMON N
  N = 2
  CALL sub()
  END

  SUBROUTINE Sub()

  INTEGER N
  COMMON N

  TYPE dt(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: arr(l)="12345"
  END TYPE

  TYPE(dt(n*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr

  IF (b%l         .NE. 4)  STOP 11
  IF (LEN(b%arr)  .NE. 4)  STOP 12
  IF (SIZE(b%arr) .NE. 4)  STOP 13

  IF (ANY(b%arr   .NE. "1234")) STOP 14

  END SUBROUTINE

