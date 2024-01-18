!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 30, 2006
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
!*  intrinsic-type-spec in select type stmt
!*
!* (325910/326233)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecSltTyp2
  IMPLICIT  CHARACTER(KIND=KIND(0_1))           (A)
  IMPLICIT  CHARACTER(LEN=A%LEN+1, KIND=A%KIND) (B)
  IMPLICIT  CHARACTER(B%LEN+1,     KIND=B%KIND) (C)

  INTEGER :: I

  CLASS(*), ALLOCATABLE :: C1(:)
  CLASS(*), ALLOCATABLE :: C2(:)
  CLASS(*), ALLOCATABLE :: C3(:)

  !ALLOCATE(CHARACTER(KIND=KIND(-1_1)) :: C1(128))
  !C1 = [(CHAR(I), I=0, 127)]
  ALLOCATE(C1(128), SOURCE=[(CHAR(I), I=0, 127)])

  ALLOCATE(C2(128), SOURCE= [CHARACTER(LEN=A%LEN+1, KIND=A%KIND):: [(CHAR(I)//CHAR(I), I=0, 127)] ])
  ALLOCATE(C3(128), SOURCE= [CHARACTER(B%LEN+1,     KIND=B%KIND):: [CHARACTER(B%LEN+1, KIND=B%KIND)::(" "//CHAR(I)//" ", I=0, 127)] ])


  SELECT TYPE ( C1 )
  !TYPE IS (CHARACTER(KIND=KIND([-1_1]),LEN=*))
  TYPE IS (CHARACTER(KIND=1,LEN=*))
    IF ( KIND(C1) .NE. 1  )                       ERROR STOP 11
    IF (  LEN(C1) .NE. 1  )                       ERROR STOP 12
    IF (  ANY(C1  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 13
  CLASS DEFAULT
    STOP 24
  END SELECT

  SELECT TYPE ( C2 )
  TYPE IS (CHARACTER(KIND=KIND([CHARACTER(LEN=A%LEN+1, KIND=A%KIND):: [(CHAR(I)//CHAR(I), I=0, 127)] ]), LEN=*))
    IF ( KIND(C2) .NE. 1  )                       ERROR STOP 21
    IF (  LEN(C2) .NE. 2  )                       ERROR STOP 22
    IF (  ANY(C2(:)(1:1)  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 23
    IF (  ANY(C2(:)(2:2)  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 24
  CLASS DEFAULT
    STOP 24
  END SELECT

  SELECT TYPE ( C3 )
  TYPE IS (CHARACTER(KIND=KIND([CHARACTER(B%LEN+1,KIND=B%KIND)::[CHARACTER(B%LEN+1,KIND=B%KIND)::(" "//CHAR(I)//" ", I=0, 127)] ]), LEN=*))
    IF ( KIND(C3) .NE. 1  )                       ERROR STOP 31
    IF (  LEN(C3) .NE. 3  )                       ERROR STOP 32
    IF (  ANY(C3(:)(1:1)  .NE. " ") )                     ERROR STOP 33
    IF (  ANY(C3(:)(2:2)  .NE. (/(CHAR(I), I=0, 127)/)) ) ERROR STOP 34
    IF (  ANY(C3(:)(3:3)  .NE. " ") )                     ERROR STOP 35
  CLASS DEFAULT
    STOP 34
  END SELECT



  END


