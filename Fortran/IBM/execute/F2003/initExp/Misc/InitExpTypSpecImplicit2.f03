!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 30, 2006
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
!*  intrinsic-type-spec in implicit stmt
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecImplicit2
  IMPLICIT  CHARACTER(KIND=KIND(0_1))           (A)
  IMPLICIT  CHARACTER(LEN=A%LEN+1, KIND=A%KIND) (B)
  IMPLICIT  CHARACTER(B%LEN+1,     KIND=B%KIND) (C)

  INTEGER :: I

  DIMENSION    :: A(128)
  PARAMETER      (A=[(CHAR(I), I=0, 127)])
  DIMENSION    :: B(128)
  PARAMETER      (B=[(CHAR(I), I=0, 127)])
  DIMENSION    :: C(128)
  PARAMETER      (C=[(CHAR(I), I=0, 127)])

  IF ( KIND(A) .NE. 1  )                       ERROR STOP 11
  IF (  LEN(A) .NE. 1  )                       ERROR STOP 12
  IF (  ANY(A  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 13

  IF ( KIND(B) .NE. 1  )                       ERROR STOP 21
  IF (  LEN(B) .NE. 2  )                       ERROR STOP 22
  IF (  ANY(B  .NE. (/(CHAR(I)//" ", I=0, 127)/)) ) ERROR STOP 23

  IF ( KIND(C) .NE. 1  )                       ERROR STOP 31
  IF (  LEN(C) .NE. 3  )                       ERROR STOP 32
  IF (  ANY(C  .NE. (/(CHAR(I)//"  ", I=0, 127)/)) ) ERROR STOP 33

  END

