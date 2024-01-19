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
!*  Record Structure -- An IBM extension
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpRecStruct
  IMPLICIT NONE

  INTEGER :: I

  STRUCTURE /S1/
    INTEGER(1) :: I1=1
    LOGICAL(1) :: L1=.TRUE.
    CHARACTER  :: C=" "
  END STRUCTURE

  RECORD /S1/ T
  PARAMETER  (T=S1())

  STRUCTURE /S2/
    BYTE :: B1(T%I1%KIND)=(/(T%I1, I=1, T%I1%KIND)/)
    BYTE :: B2(T%L1%KIND)=(/(T%L1, I=1, T%L1%KIND)/)
    BYTE :: B3(T%C%KIND) =(/(T%C,  I=1, T%C%KIND)/)
  END STRUCTURE

  RECORD /S2/ T1


  IF (SIZE(T1%B1)    .NE. 1  ) ERROR STOP 11
  IF (SIZE(T1%B2)    .NE. 1  ) ERROR STOP 12
  IF (SIZE(T1%B3)    .NE. 1  ) ERROR STOP 13

  IF (ANY( T1%B1     .NE.   1))        ERROR STOP 61
  IF (ANY( T1%B2     .NEQV. .TRUE. ))  ERROR STOP 62
  IF (ANY( T1%B3     .NE.   " "))      ERROR STOP 63

  END


