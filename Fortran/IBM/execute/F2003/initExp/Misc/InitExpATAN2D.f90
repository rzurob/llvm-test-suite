!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 17, 2006
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
!*  ATAN2DD -- An IBM extension
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpATAN2D
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4), PARAMETER :: Y4(2,2) = RESHAPE((/1.0,-1.0,1.0,-1.0/),(/2,2/))
  REAL(8), PARAMETER :: Y8(2,2) = RESHAPE((/1.0,-1.0,1.0,-1.0/),(/2,2/))
  REAL(16),PARAMETER :: Y6(2,2) = RESHAPE((/1.0,-1.0,1.0,-1.0/),(/2,2/))

  REAL(4), PARAMETER :: X4(2,2) = RESHAPE((/-1.0,-1.0,1.0,1.0/),(/2,2/))
  REAL(8), PARAMETER :: X8(2,2) = RESHAPE((/-1.0,-1.0,1.0,1.0/),(/2,2/))
  REAL(16),PARAMETER :: X6(2,2) = RESHAPE((/-1.0,-1.0,1.0,1.0/),(/2,2/))

  REAL(16),PARAMETER :: Result(2,2) =  RESHAPE((/135.0, -135.0, 45.0, -45.0/),(/2,2/))

  LOGICAL precision_R6, precision_R8

  REAL(KIND(ATAN2D(Y4,X4))), PARAMETER :: V4(2,2) =  RESHAPE((/ATAN2D(Y4,X4)/),(/2,2/))
  REAL(KIND(ATAN2D(Y8,X8))), PARAMETER :: V8(2,2) =  RESHAPE((/ATAN2D(Y8,X8)/),(/2,2/))
  REAL(KIND(ATAN2D(Y6,X6))), PARAMETER :: V6(2,2) =  RESHAPE((/ATAN2D(Y6,X6)/),(/2,2/))

  REAL(KIND(DATAN2D(Y8,X8))), PARAMETER :: DV(2,2) =  RESHAPE((/DATAN2D(Y8,X8)/),(/2,2/))
  REAL(KIND(QATAN2D(Y6,X6))), PARAMETER :: QV(2,2) =  RESHAPE((/QATAN2D(Y6,X6)/),(/2,2/))



  IF (KIND(V4)     .NE. 4   )                       ERROR STOP 11
  IF ( ANY(V4      .NE. Result))                    ERROR STOP 12

  IF (KIND(V8)     .NE. 8   )                       ERROR STOP 21
  IF ( ANY(V8      .NE. Result))                    ERROR STOP 22

  IF (KIND(V6)     .NE. 16  )                       ERROR STOP 31
! IF ( ANY(V6      .NE. Result))                    ERROR STOP 32

  IF (KIND(DV)     .NE. 8   )                       ERROR STOP 41
  IF ( ANY(DV      .NE. Result))                    ERROR STOP 42

  IF (KIND(QV)     .NE. 16  )                       ERROR STOP 51
! IF ( ANY(QV      .NE. Result))                    ERROR STOP 52

  DO I = 1, 2
  DO J = 1, 2
    IF (.NOT. precision_R6(V6(I,J), Result(I,J)) )                   ERROR STOP 32
    IF (.NOT. precision_R6(QV(I,J), Result(I,J)) )                   ERROR STOP 52
  END DO
  END DO

  END


