!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Initialize components
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, 4)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp
  USE M

  TYPE(DT1(4,1))              :: T
  TYPE(DT1(8,:)), POINTER     :: P
  TYPE(DT1(2,:)), ALLOCATABLE :: A

  IF ( KIND(T%I) .NE. 4 )      ERROR STOP 11
  IF ( T%I      .NE. 4 )       ERROR STOP 12
  IF ( LEN(T%C) .NE. 1 )       ERROR STOP 13
  IF ( T%C      .NE. "!" )     ERROR STOP 14
  IF ( ASSOCIATED(T%ProcPtr) ) ERROR STOP 15

  ALLOCATE(DT1(8,2) :: P )
  IF ( KIND(P%I) .NE. 8 )      ERROR STOP 21
  IF ( P%I      .NE. 8 )       ERROR STOP 22
  IF ( LEN(P%C) .NE. 2 )       ERROR STOP 23
  IF ( P%C      .NE. "!!" )    ERROR STOP 24
  IF ( ASSOCIATED(P%ProcPtr) ) ERROR STOP 25

  ALLOCATE(DT1(2,1+2) :: A )
  IF ( KIND(A%I) .NE. 2 )      ERROR STOP 31
  IF ( A%I      .NE. 2 )       ERROR STOP 32
  IF ( LEN(A%C) .NE. 3 )       ERROR STOP 33
  IF ( A%C      .NE. "!!!" )   ERROR STOP 34
  IF ( ASSOCIATED(A%ProcPtr) ) ERROR STOP 35

  END
