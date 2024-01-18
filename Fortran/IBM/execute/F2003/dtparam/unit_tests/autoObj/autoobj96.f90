!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2009
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
!*  Char array comp and array dummy
!*  (RTO issue)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj05
  CALL sub(2)
  CONTAINS

  SUBROUTINE Sub(N)
  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c(l)
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l)
  END TYPE

  TYPE(dt(n)) b (n)


  IF (b%l                .NE. 2)  STOP 11
  IF (b(1)%arr%l         .NE. 2)  STOP 12

  IF (lbound(b(1)%arr,1) .NE. 1)  STOP 13
  IF (ubound(b(1)%arr,1) .NE. 2)  STOP 14
  IF (SIZE(b(1)%arr)     .NE. 2)  STOP 15

  IF (lbound(b(1)%arr(n)%c,1) .NE. 1)  STOP 16
  IF (ubound(b(1)%arr(n)%c,1) .NE. 2)  STOP 17
  IF (len(b(1)%arr(1)%c)      .NE. 2)  STOP 18

  b(1)%arr(n)%c = '123'
  b(n)%arr(1)%c = '321'
  IF (ANY(b(1)%arr(n)%c       .NE. "12")) STOP 19
  IF (ANY(b(n)%arr(1)%c       .NE. "32")) STOP 20

  END SUBROUTINE
  END

