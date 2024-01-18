!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  intrinsic-type-spec : CHARACTER
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpeChar
  IMPLICIT NONE

  INTEGER :: I

  CHARACTER(1), POINTER :: C(:)

  CHARACTER(KIND=C%KIND)             :: C1(128)=(/(CHAR(I), I=0, 127)/)
  CHARACTER(LEN=C%LEN,  KIND=C%KIND) :: C2(128)=(/(CHAR(I), I=0, 127)/)
  CHARACTER(C%LEN,  KIND=C%KIND)     :: C3(128)=(/(CHAR(I), I=0, 127)/)

  IF ( KIND(C1) .NE. 1  )                       ERROR STOP 11
  IF (  LEN(C1) .NE. 1  )                       ERROR STOP 12
  IF (  ANY(C1  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 13

  IF ( KIND(C2) .NE. 1  )                       ERROR STOP 21
  IF (  LEN(C2) .NE. 1  )                       ERROR STOP 22
  IF (  ANY(C2  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 23

  IF ( KIND(C3) .NE. 1  )                       ERROR STOP 31
  IF (  LEN(C3) .NE. 1  )                       ERROR STOP 32
  IF (  ANY(C3  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 33

  END


