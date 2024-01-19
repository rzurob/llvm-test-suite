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
!* Initialize array components
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
      INTEGER(K)   :: I(K)=K
      REAL(K)      :: R(K)=K
      COMPLEX(K)   :: Z(K)=(K,-K)
      CHARACTER(K) :: C(K) = CHAR(K)  !C(L) = CHAR(K)
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, arg%l)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp
  USE M

  TYPE(DT1(4,1))              :: T
  TYPE(DT1(8,:)), POINTER     :: P
  TYPE(DT1(4,:)), ALLOCATABLE :: A

  IF ( KIND(T%I) .NE. 4 )                 ERROR STOP 11
  IF ( SIZE(T%I) .NE. 4 )                 ERROR STOP 12
  IF ( ANY(T%I  .NE. 4))     ERROR STOP 13
  IF ( KIND(T%R) .NE. 4 )                 ERROR STOP 14
  IF ( SIZE(T%R) .NE. 4 )                 ERROR STOP 16
  IF ( ANY(T%R  .NE. 4))     ERROR STOP 17
  IF ( KIND(T%Z) .NE. 4 )                 ERROR STOP 18
  IF ( SIZE(T%Z) .NE. 4 )                 ERROR STOP 19
  IF ( ANY(T%Z  .NE. (4,-4))) ERROR STOP 20
  IF ( LEN(T%C) .NE. 4 )                  ERROR STOP 21
  IF ( SIZE(T%C).NE. 4 )                  ERROR STOP 22
  IF ( ANY(T%C  .NE. CHAR(4)//"   " ))    ERROR STOP 23
  IF ( ASSOCIATED(T%ProcPtr) )            ERROR STOP 24

  ALLOCATE(DT1(8,2) :: P )
  IF ( KIND(P%I) .NE. 8 )                 ERROR STOP 31
  IF ( SIZE(P%I) .NE. 8 )                 ERROR STOP 32
  IF ( ANY(P%I  .NE. 8))     ERROR STOP 33
  IF ( KIND(P%R) .NE. 8 )                 ERROR STOP 34
  IF ( SIZE(P%R) .NE. 8 )                 ERROR STOP 36
  IF ( ANY(P%R  .NE. 8))     ERROR STOP 37
  IF ( KIND(P%Z) .NE. 8 )                 ERROR STOP 38
  IF ( SIZE(P%Z) .NE. 8 )                 ERROR STOP 39
  IF ( ANY(P%Z  .NE. (8,-8))) ERROR STOP 40
  IF ( LEN(P%C) .NE. 8 )                  ERROR STOP 41
  IF ( SIZE(P%C).NE. 8 )                  ERROR STOP 42
  IF ( ANY(P%C  .NE. CHAR(8)//"       " ))ERROR STOP 43
  IF ( ASSOCIATED(P%ProcPtr) )            ERROR STOP 44
  DEALLOCATE( P )

  ALLOCATE(DT1(4,1+2) :: A )
  IF ( KIND(A%I) .NE. 4 )                 ERROR STOP 51
  IF ( SIZE(A%I) .NE. 4 )                 ERROR STOP 52
  IF ( ANY(A%I  .NE. 4))     ERROR STOP 53
  IF ( KIND(A%R) .NE. 4 )                 ERROR STOP 54
  IF ( SIZE(A%R) .NE. 4 )                 ERROR STOP 56
  IF ( ANY(A%R  .NE. 4))     ERROR STOP 57
  IF ( KIND(A%Z) .NE. 4 )                 ERROR STOP 58
  IF ( SIZE(A%Z) .NE. 4 )                 ERROR STOP 59
  IF ( ANY(A%Z  .NE. (4,-4))) ERROR STOP 60
  IF ( LEN(A%C) .NE. 4 )                  ERROR STOP 61
  IF ( SIZE(A%C).NE. 4 )                  ERROR STOP 62
  IF ( ANY(A%C  .NE. CHAR(4)//" " ))      ERROR STOP 63
  IF ( ASSOCIATED(A%ProcPtr) )            ERROR STOP 64
  DEALLOCATE( A )

  END

