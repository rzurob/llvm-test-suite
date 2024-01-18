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
!*  ATAND  -- An IBM extension
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpATAND
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4), PARAMETER :: X4(45) = -1.0
  REAL(8), PARAMETER :: X8(45) = -1.0
  REAL(16),PARAMETER :: X6(45) = -1.0

  TYPE :: DT

    REAL(KIND(ATAND(X4))) :: R4(ABS(INT(ATAND(X=X4(1))))) = (/ATAND(X=X4)/)
    REAL(KIND(ATAND(X8))) :: R8(ABS(INT(ATAND(X=X8(1))))) = (/ATAND(X=X8)/)
    REAL(KIND(ATAND(X6))) :: R6(ABS(INT(ATAND(X=X6(1)-0.000001)))) = (/ATAND(X=X6)/)

    REAL(KIND(DATAND(X8))):: DR(ABS(INT(DATAND(X=X8(1))))) = (/DATAND(X=X8)/)
    REAL(KIND(QATAND(X6))):: QR(ABS(INT(QATAND(X=X6(1)-0.000001)))) = (/QATAND(X=X6)/)

  END TYPE

  TYPE(DT) :: T
  LOGICAL precision_R6, precision_R8

  IF (KIND(T%R4)     .NE. 4   )                       STOP 11
  IF (SIZE(T%R4)     .NE. 45  )                       STOP 12
  IF ( ANY(T%R4      .NE. -45.0))                     STOP 13

  IF (KIND(T%R8)     .NE. 8   )                       STOP 21
  IF (SIZE(T%R8)     .NE. 45  )                       STOP 22
  IF ( ANY(T%R8      .NE. -45.0))                     STOP 23

  IF (KIND(T%R6)     .NE. 16  )                       STOP 31
  IF (SIZE(T%R6)     .NE. 45  )                       STOP 32
 !IF ( ANY(T%R6      .NE. -45.0))                     STOP 33

  IF (KIND(T%DR)     .NE. 8   )                       STOP 41
  IF (SIZE(T%DR)     .NE. 45  )                       STOP 42
 !IF ( ANY(T%DR      .NE. -45.0))                     STOP 43

  IF (KIND(T%QR)     .NE. 16  )                       STOP 51
  IF (SIZE(T%QR)     .NE. 45  )                       STOP 52
 !IF ( ANY(T%QR      .NE. -45.0))                     STOP 53

  DO I = 1, 45
    IF (.NOT. precision_R6(T%R6(I), -45.0Q0))                    STOP 33
    IF (.NOT. precision_R8(T%DR(I), -45.0Q0))                    STOP 43
    IF (.NOT. precision_R6(T%QR(I), -45.0Q0))                    STOP 53
  END DO

  END


