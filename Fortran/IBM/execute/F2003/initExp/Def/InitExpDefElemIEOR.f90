!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemIEOR.f
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
!*  -  IEOR
!*  (319568)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIEOR
  PARAMETER ( K8=8 )
  !IMPLICIT INTEGER(SIZE(IEOR((/(J,J=1,K8)/),K8)))(A)
  IMPLICIT INTEGER(SIZE(IEOR((/(J,J=1,K8)/),K8 )))(A)
  !IMPLICIT INTEGER(IEOR(K8,K8 ))(A)

  INTEGER :: I, J


  INTEGER(1),                      PARAMETER :: IC11(128)=(/(7, I=0, 127)/)
  INTEGER(1),                      PARAMETER :: IC12(128)=(/(6, I=0, 127)/)
  INTEGER(KIND(IEOR(IC11, IC12))), PARAMETER :: IC13(128)=IEOR(IC11, IC12)

  INTEGER(2),                      PARAMETER :: IC21(128)=(/(7, I=0, 127)/)
  INTEGER(2),                      PARAMETER :: IC22(128)=(/(5, I=0, 127)/)
  INTEGER(KIND(IEOR(IC21, IC22))), PARAMETER :: IC23(128)=IEOR(I=IC21, J=IC22)

  INTEGER(4),                      PARAMETER :: IC41(128)=(/(7, I=0, 127)/)
  INTEGER(4),                      PARAMETER :: IC42(128)=(/(3, I=0, 127 )/)
  INTEGER(KIND(IEOR(IC41, IC42))), PARAMETER :: IC43(128)=IEOR(IC41, J=IC42)

  INTEGER(8),                      PARAMETER :: IC81(128)=(/(15, I=0, 127)/)
  INTEGER(8),                      PARAMETER :: IC82(128)=(/(7,  I=0, 127)/)
  INTEGER(KIND(IEOR(IC81, IC82))), PARAMETER :: IC83(128)=IEOR(J=IC81, I=IC82)

  CLASS(*), ALLOCATABLE :: R(:)

  IF ( KIND(A)    .NE. 8 )         STOP 10

  IF ( KIND(IC13) .NE. 1 )         STOP 11
  IF ( ANY( IC13  .NE. 1 ))        STOP 12

  IF ( KIND(IC23) .NE. 2 )         STOP 21
  IF ( ANY( IC23  .NE. 2 ))        STOP 22

  IF ( KIND(IC43) .NE. 4 )         STOP 41
  IF ( ANY( IC43  .NE. 4 ))        STOP 42

  IF ( KIND(IC83) .NE. 8 )         STOP 81
  IF ( ANY( IC83  .NE. 8 ))        STOP 82

  ALLOCATE(R(3), SOURCE=16._16)

  SELECT TYPE ( As => R)
  TYPE IS (REAL(IEOR(J=7, I=3)))
    STOP 51
  TYPE IS (REAL(IEOR(J=15, I=7)))
    STOP 52
  TYPE IS (REAL(IEOR(I=31, J=15)))
    PRINT*, As
  CLASS DEFAULT
    STOP 53
  END SELECT

  END


