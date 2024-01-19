!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 24, 2006
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
!*  - CHAR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCHAR
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER,      PARAMETER :: IC1(3)=ICHAR("1")
  INTEGER,      PARAMETER :: IC2(3)=ICHAR("2")
  INTEGER,      PARAMETER :: IC3(3)=ICHAR("3")
  CHARACTER(3), PARAMETER :: C1=CHAR(ICHAR("1"))//CHAR(ICHAR("2"))//CHAR(ICHAR("3"))
  CHARACTER(3), PARAMETER :: C2(3)=(/(C1(1:1)//CHAR(ICHAR("2"))//C1(3:3), I=1,3)/)
  CHARACTER(3), PARAMETER :: C3(3)=CHAR(IC1(:))//CHAR(IC2(:))//CHAR(IC3(:))
  CHARACTER(3), PARAMETER :: C4(3)=(/C3(1:2), C3(3:3)/)

  INTERFACE
    FUNCTION ExtFun(Arg) BIND(C, NAME=CHAR(ICHAR("c")))
      CHARACTER(1) :: ExtFun, Arg
    END FUNCTION
  END INTERFACE

  IF (ANY( IC1                   .NE. ICHAR("1"))) ERROR STOP 11
  IF (ANY( IC2                   .NE. ICHAR("2"))) ERROR STOP 11
  IF (ANY( IC3                   .NE. ICHAR("3"))) ERROR STOP 11
  IF (C1                         .NE. "123")       ERROR STOP 12
  IF (ANY( C2                    .NE. "123"))      ERROR STOP 13
  IF (ANY( C3                    .NE. "123"))      ERROR STOP 14
  IF (ANY( C4                    .NE. "123"))      ERROR STOP 15
  IF ( ExtFun(CHAR(ICHAR("1")) ) .NE. "1")         ERROR STOP 16

  END

  FUNCTION ExtFun(Arg) BIND(C, NAME=CHAR(ICHAR("c")))
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION


