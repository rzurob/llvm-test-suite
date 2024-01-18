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

  IF ( KIND(T%I) .NE. 4 )      STOP 11
  IF ( T%I      .NE. 4 )       STOP 12
  IF ( LEN(T%C) .NE. 1 )       STOP 13
  IF ( T%C      .NE. "!" )     STOP 14
  IF ( ASSOCIATED(T%ProcPtr) ) STOP 15

  ALLOCATE(DT1(8,2) :: P )
  IF ( KIND(P%I) .NE. 8 )      STOP 21
  IF ( P%I      .NE. 8 )       STOP 22
  IF ( LEN(P%C) .NE. 2 )       STOP 23
  IF ( P%C      .NE. "!!" )    STOP 24
  IF ( ASSOCIATED(P%ProcPtr) ) STOP 25

  ALLOCATE(DT1(2,1+2) :: A )
  IF ( KIND(A%I) .NE. 2 )      STOP 31
  IF ( A%I      .NE. 2 )       STOP 32
  IF ( LEN(A%C) .NE. 3 )       STOP 33
  IF ( A%C      .NE. "!!!" )   STOP 34
  IF ( ASSOCIATED(A%ProcPtr) ) STOP 35

  END

