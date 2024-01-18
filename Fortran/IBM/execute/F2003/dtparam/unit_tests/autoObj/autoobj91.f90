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
!*  char component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj02

  TYPE dt(l)
    INTEGER, LEN :: l
  ! CHARACTER(l) :: arr="12345"
    CHARACTER(l) :: arr(l)="12345"
  END TYPE

  CALL sub(4)
  CONTAINS

  SUBROUTINE Sub(N)
  INTEGER N
  TYPE(dt(N)) b(N)

    IF (b%l                 .NE. 4)       ERROR STOP 11
    IF (UBOUND(b(1)%arr, 1) .NE. 4)       ERROR STOP 12
    IF (SIZE(b(1)%arr)      .NE. 4)       ERROR STOP 13
    IF (ANY(b(1)%arr        .NE. "1234")) ERROR STOP 14
    IF (ANY(b%arr(1)        .NE. "1234")) ERROR STOP 15

  END SUBROUTINE
  END

