!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefConst1.f
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
!*  - ENUM
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpDefConst1
  IMPLICIT NONE


  ENUM, BIND(C)
    ENUMERATOR :: Zero
    ENUMERATOR :: One
    ENUMERATOR :: Two
    ENUMERATOR :: Thr
  END ENUM

  ENUM, BIND(C)
    ENUMERATOR :: Start
    ENUMERATOR :: Mon
    ENUMERATOR :: Tue
    ENUMERATOR :: Wed
    ENUMERATOR :: Thu
    ENUMERATOR :: Fri
    ENUMERATOR :: Sat
    ENUMERATOR :: Sun
  END ENUM

  TYPE :: DT
    INTEGER       :: ID1
    REAL          :: ID2
  END TYPE

  INTEGER :: I, J

  TYPE(DT), PARAMETER ::  C=DT(ID2=Mon, ID1=Thr), CArr(10)=C

  TYPE :: DT1
    TYPE(DT) :: Comp(10)
  END TYPE

  TYPE(DT1), PARAMETER ::  C1=DT1((/(C, I=1, 10)/)), CArr1(10)=(/(C1, I=1, 10)/)

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2), PARAMETER ::  C2=DT2(DT=C1%Comp(One)), CArr2(10)=DT2(DT=CArr1(One)%Comp(One))

  IF (C%ID1         .NE. Thr  ) STOP 11
  IF (C%ID2         .NE. Mon  ) STOP 12

  IF (ANY(CArr%ID1   .NE. (/(Thr, I=1, 10)/)) )            STOP 21
  IF (ANY(CArr%ID2   .NE. (/(Mon, I=1, 10)/)) )            STOP 22

  IF (ANY(C1%Comp%ID1          .NE. Thr ) )           STOP 31
  IF (ANY(C1%Comp%ID2          .NE. Mon ) )           STOP 32

  DO J=1, 10
    IF (ANY(CArr1(J)%Comp%ID1  .NE. Thr) )            STOP 41
    IF (ANY(CArr1(J)%Comp%ID2  .NE. Mon) )            STOP 42
  END DO

  IF (C2%ID1          .NE. Thr  )           STOP 51
  IF (C2%ID2          .NE. Mon  )           STOP 52

  DO J=1, 10
    IF (CArr2(J)%ID1  .NE. Thr  )           STOP 61
    IF (CArr2(J)%ID2  .NE. Mon  )           STOP 62
  END DO

  END


