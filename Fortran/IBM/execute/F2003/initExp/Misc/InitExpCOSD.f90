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
!*  COSD  -- An IBM extension
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    REAL(KIND((/COSD(X=-60.)/)))      :: R4(1)= (/COSD(X=-60.)/)
    REAL(KIND((/COSD(X=-120.D0)/)))   :: R8(1)= (/COSD(X=-120.D0)/)
    REAL(KIND((/COSD(X=-240.Q0)/)))   :: R6(1)= (/COSD(X=-240.Q0)/)
    PROCEDURE(),NOPASS, POINTER       :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, PASS ::  ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpCOSD
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE(DT), PARAMETER :: Param=DT()


  TYPE(DT) ::  T,Arr1(1)
  PARAMETER  (  Arr1=(/DT(R4=(/COSD(X=60.)/), R8=(/COSD(X=120.D0)/), R6=(/COSD(X=240.Q0)/))/) )

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE(DT1) :: Arr2(1) =(/DT1(DT=DT(R4=(/COSD(X=360.)/), R8=(/COSD(X=-360.0)/), R6=(/COSD(X=0.Q0)/)))/)

  TYPE(DT) ::  T1=DT(R8=(/DCOSD(X=360.D0)/), R6=(/QCOSD(X=360.Q0)/))

  LOGICAL precision_R8
  LOGICAL precision_R6

  IF (KIND(T%R4)     .NE.  4 )                    STOP 11
  IF ( ANY(T%R4      .NE.  0.5))                  STOP 12

  IF (KIND(T%R8)     .NE.  8 )                    STOP 21
 !IF ( ANY(T%R8      .NE. -0.5))                  STOP 22
  IF ( .NOT. precision_R8(T%R8(1), -0.5D0))       STOP 22

  IF (KIND(T%R6)     .NE. 16 )                    STOP 31
 !IF ( ANY(T%R6      .NE. -0.5))                  STOP 32
  IF ( .NOT. precision_R6(T%R6(1), -0.5Q0))       STOP 32


  IF (KIND(Arr1(1)%R4)   .NE.  4 )                STOP 41
  IF ( ANY(Arr1(1)%R4    .NE.  0.5))              STOP 42

  IF (KIND(Arr1(1)%R8)   .NE.  8 )                STOP 51
 !IF ( ANY(Arr1(1)%R8    .NE. -0.5))              STOP 52
  IF ( .NOT. precision_R8(Arr1(1)%R8(1), -0.5D0)) STOP 52

  IF (KIND(Arr1(1)%R6)   .NE. 16 )                STOP 61
 !IF ( ANY(Arr1(1)%R6    .NE. -0.5))              STOP 62
  IF ( .NOT. precision_R6(Arr1(1)%R6(1), -0.5Q0)) STOP 62


  IF (KIND(Arr2(1)%R4)   .NE.  4 )                STOP 71
 !IF ( ANY(Arr2(1)%R4    .NE.  1))                STOP 72
  IF ( .NOT. precision_R8(Arr2(1)%R8(1), 1.D0))   STOP 72

  IF (KIND(Arr2(1)%R8)   .NE.  8 )                STOP 81
 !IF ( ANY(Arr2(1)%R8    .NE.  1))                STOP 82
  IF ( .NOT. precision_R8(Arr2(1)%R8(1), 1D0))    STOP 82

  IF (KIND(Arr2(1)%R6)   .NE. 16 )                STOP 91
 !IF ( ANY(Arr2(1)%R6    .NE.  1.))               STOP 92
  IF ( .NOT. precision_R6(Arr2(1)%R6(1), 1.Q0))   STOP 92


 !IF ( ANY(T1%R8    .NE.  1))                STOP 182
  IF ( .NOT. precision_R8(T1%R8(1), 1D0))    STOP 182

 !IF ( ANY(T1%R6    .NE.  1.))               STOP 192
  IF ( .NOT. precision_R6(T1%R6(1), 1.Q0))   STOP 192

  END


