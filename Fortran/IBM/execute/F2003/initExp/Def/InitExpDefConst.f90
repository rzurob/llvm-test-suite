!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 17, 2006
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
!*  the primary is a constant/ subobject of a constant
!*
!*  (ICE-318836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpDefConst
  IMPLICIT NONE

  TYPE :: DT
    CHARACTER(3)  :: Str="MBI"
    INTEGER       :: ID=-1
    REAL, POINTER :: RPtr=>NULL()
  END TYPE

  INTEGER :: I, J

  TYPE(DT), PARAMETER ::  C=DT(), CArr(10)=(/(DT(ID=i, Str="IBM"), I=1, 10)/)

  TYPE :: DT1
    TYPE(DT) :: Comp(10)
  END TYPE

  TYPE(DT1), PARAMETER ::  C1=DT1(CArr), CArr1(10)=(/(DT1(CArr), I=1, 10)/)

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2), PARAMETER ::  C2=DT2(DT=C1%Comp(1)), CArr2(10)=DT2(DT=CArr1(1)%Comp(1))


  IF (C%Str         .NE. "MBI" ) ERROR STOP 11
  IF (C%ID          .NE. -1    ) ERROR STOP 12

  IF (ANY(CArr%Str  .NE. "IBM") )                       ERROR STOP 21
  IF (ANY(CArr%ID   .NE. (/(I, I=1, 10)/)) )            ERROR STOP 22

  IF (ANY(C1%Comp%Str          .NE. "IBM"  ))           ERROR STOP 31
  IF (ANY(C1%Comp%ID           .NE. CArr%ID))           ERROR STOP 32

  DO J=1, 10
    IF (ANY(CArr1(J)%Comp%Str  .NE. "IBM") )            ERROR STOP 41
    IF (ANY(CArr1(J)%Comp%ID   .NE. (/(I, I=1, 10)/)) ) ERROR STOP 42
  END DO

  IF (C2%Str          .NE. "IBM"  )           ERROR STOP 51
  IF (C2%ID           .NE. 1     )            ERROR STOP 52

  IF (ANY(CArr2%Str  .NE. "IBM") )            ERROR STOP 61
  IF (ANY(CArr2%ID   .NE. 1 ) )               ERROR STOP 62

  END


