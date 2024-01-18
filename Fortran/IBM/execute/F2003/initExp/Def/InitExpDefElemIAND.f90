!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemIAND.f
!*
!*  DATE                       : Apr. 07, 2006
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
!*  -  IAND
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIAND
  IMPLICIT INTEGER(KIND(IAND((/(J,J=1,16)/),(/(J,J=1,16)/))))(A)
  INTEGER :: I, J

  INTEGER(1),                      PARAMETER :: IC11(128)=(/(1,0, I=0, 127, 2)/)
  INTEGER(1),                      PARAMETER :: IC12(128)=(/(0,1, I=0, 127, 2)/)
  INTEGER(KIND(IAND(IC11, IC12))), PARAMETER :: IC13(128)=IAND(IC11, IC12)

  INTEGER(2),                      PARAMETER :: IC21(128)=(/(1,0, I=0, 127, 2)/)
  INTEGER(2),                      PARAMETER :: IC22(128)=(/(0,1, I=0, 127, 2)/)
  INTEGER(KIND(IAND(IC21, IC22))), PARAMETER :: IC23(128)=IAND(IC21, IC22)

  INTEGER(4),                      PARAMETER :: IC41(128)=(/(1,0, I=0, 127, 2)/)
  INTEGER(4),                      PARAMETER :: IC42(128)=(/(0,1, I=0, 127, 2)/)
  INTEGER(KIND(IAND(IC41, IC42))), PARAMETER :: IC43(128)=IAND(IC41, IC42)

  INTEGER(8),                      PARAMETER :: IC81(128)=(/(1,0, I=0, 127, 2)/)
  INTEGER(8),                      PARAMETER :: IC82(128)=(/(0,1, I=0, 127, 2)/)
  INTEGER(KIND(IAND(IC81, IC82))), PARAMETER :: IC83(128)=IAND(IC81, IC82)


  IF ( KIND(A)    .NE. 4 )         STOP 10

  IF ( KIND(IC13) .NE. 1 )         STOP 11
  IF ( ANY( IC13  .NE. 0 ))        STOP 12

  IF ( KIND(IC23) .NE. 2 )         STOP 21
  IF ( ANY( IC23  .NE. 0 ))        STOP 22

  IF ( KIND(IC43) .NE. 4 )         STOP 41
  IF ( ANY( IC43  .NE. 0 ))        STOP 42

  IF ( KIND(IC83) .NE. 8 )         STOP 81
  IF ( ANY( IC83  .NE. 0 ))        STOP 82




  END


