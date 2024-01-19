!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 10, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  IOR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIOR
  IMPLICIT  NONE
  INTEGER :: I, J

  INTEGER,                         PARAMETER :: F=127

  INTEGER(1),                      PARAMETER :: IC11(128)=(/(I, I=0, 127)/)
  INTEGER(1),                      PARAMETER :: IC12(128)=(/(127-I, I=0, 127)/)
  INTEGER(KIND(IOR(IC11, IC12))),  PARAMETER :: IC13(128)=IOR(IC11, IC12)

  INTEGER(2),                      PARAMETER :: IC21(128)=(/(I, I=0, 127)/)
  INTEGER(2),                      PARAMETER :: IC22(128)=(/(127-I, I=0, 127)/)
  INTEGER(KIND(IOR(IC21, IC22))),  PARAMETER :: IC23(128)=IOR(I=IC21, J=IC22(:))

  INTEGER(4),                      PARAMETER :: IC41(128)=(/(I, I=0, 127)/)
  INTEGER(4),                      PARAMETER :: IC42(128)=(/(127-I, I=0, 127 )/)
  INTEGER(KIND(IOR(IC41, IC42))),  PARAMETER :: IC43(128)=IOR(IC41(1:), J=IC42)

  INTEGER(8),                      PARAMETER :: IC81(128)=(/(I, I=0, 127)/)
  INTEGER(8),                      PARAMETER :: IC82(128)=(/(127-I, I=0, 127)/)
  INTEGER(KIND(IOR(IC81, IC82))),  PARAMETER :: IC83(128)=IOR(J=IC81(:), I=IC82(:))

  LOGICAL :: T1 = ANY(IOR(J=IC81(:), I=IC82(:)) .NE. F)
  LOGICAL :: T2 = ALL(IOR(J=IC81(:), I=IC82(:)) .EQ. F)

  IF ( KIND(IC13) .NE. 1 )         ERROR STOP 11
  IF ( ANY( IC13  .NE. F ))        ERROR STOP 12

  IF ( KIND(IC23) .NE. 2 )         ERROR STOP 21
  IF ( ANY( IC23  .NE. F ))        ERROR STOP 22

  IF ( KIND(IC43) .NE. 4 )         ERROR STOP 41
  IF ( ANY( IC43  .NE. F ))        ERROR STOP 42

  IF ( KIND(IC83) .NE. 8 )         ERROR STOP 81
  IF ( ANY( IC83  .NE. F ))        ERROR STOP 82

  IF ( (T1 .OR. .NOT. T2) .EQV. .TRUE. ) ERROR STOP 91

  END


