!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 28, 2006
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
!*  -- Byte
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpByte
  IMPLICIT NONE

  INTEGER :: I

  TYPE :: DT
    INTEGER(1) :: I1=1
    LOGICAL(1) :: L1=.TRUE.
    CHARACTER  :: C=" "
  END TYPE

  TYPE(DT), PARAMETER :: T=DT()

  TYPE :: DT1
    BYTE :: B1(T%I1%KIND)=(/(T%I1, I=1, T%I1%KIND)/)
    BYTE :: B2(T%L1%KIND)=(/(T%L1, I=1, T%L1%KIND)/)
    BYTE :: B3(T%C%KIND) =(/(T%C,  I=1, T%C%KIND)/)
  END TYPE

  TYPE(DT1) :: T1


  IF (SIZE(T1%B1)    .NE. 1  ) STOP 11
  IF (SIZE(T1%B2)    .NE. 1  ) STOP 12
  IF (SIZE(T1%B3)    .NE. 1  ) STOP 13

  IF (ANY( T1%B1     .NE.   1))        STOP 61
  IF (ANY( T1%B2     .NEQV. .TRUE. ))  STOP 62
  IF (ANY( T1%B3     .NE.   " "))      STOP 63

  END


