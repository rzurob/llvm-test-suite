!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp1_1 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 25, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* Initialize array components 
!* -- Not array constructor 
!*
!*  (341021) 
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTEGER, PARAMETER :: N(128)=[(I, I=1, 128)]
    complex, PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]

    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(K)=N(1:K)
      REAL(K)      :: R(K)=N(1:K)
      COMPLEX(K)   :: Z(K)=ZN(1:K)
      CHARACTER(L) :: C(K) = CHAR(K)  !C(L) = CHAR(K)
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
  TYPE(DT1(4,:)), ALLOCATABLE :: A
 
  IF ( KIND(T%I) .NE. 4 )                 STOP 11
  IF ( SIZE(T%I) .NE. 4 )                 STOP 12
  IF ( ANY(T%I  .NE. (/(I, I=1,4)/)))     STOP 13
  IF ( KIND(T%R) .NE. 4 )                 STOP 14
  IF ( SIZE(T%R) .NE. 4 )                 STOP 16
  IF ( ANY(T%R  .NE. (/(I, I=1,4)/)))     STOP 17
  IF ( KIND(T%Z) .NE. 4 )                 STOP 18
  IF ( SIZE(T%Z) .NE. 4 )                 STOP 19
  IF ( ANY(T%Z  .NE. (/((I,-I),I=1,4)/))) STOP 20
  IF ( LEN(T%C) .NE. 1 )                  STOP 21
  IF ( SIZE(T%C).NE. 4 )                  STOP 22
  IF ( ANY(T%C  .NE. CHAR(4)//"   " ))    STOP 23
  IF ( ASSOCIATED(T%ProcPtr) )            STOP 24

  ALLOCATE(DT1(8,2) :: P )
  IF ( KIND(P%I) .NE. 8 )                 STOP 31
  IF ( SIZE(P%I) .NE. 8 )                 STOP 32
  IF ( ANY(P%I  .NE. (/(I, I=1,8)/)))     STOP 33
  IF ( KIND(P%R) .NE. 8 )                 STOP 34
  IF ( SIZE(P%R) .NE. 8 )                 STOP 36
  IF ( ANY(P%R  .NE. (/(I, I=1,8)/)))     STOP 37
  IF ( KIND(P%Z) .NE. 8 )                 STOP 38
  IF ( SIZE(P%Z) .NE. 8 )                 STOP 39
  IF ( ANY(P%Z  .NE. (/((I,-I),I=1,8)/))) STOP 40
  IF ( LEN(P%C) .NE. 2 )                  STOP 41
  IF ( SIZE(P%C).NE. 8 )                  STOP 42
  IF ( ANY(P%C  .NE. CHAR(8)//"       " ))STOP 43
  IF ( ASSOCIATED(P%ProcPtr) )            STOP 44
  DEALLOCATE( P )

  ALLOCATE(DT1(4,1+2) :: A )
  IF ( KIND(A%I) .NE. 4 )                 STOP 51
  IF ( SIZE(A%I) .NE. 4 )                 STOP 52
  IF ( ANY(A%I  .NE. (/(I, I=1,4)/)))     STOP 53
  IF ( KIND(A%R) .NE. 4 )                 STOP 54
  IF ( SIZE(A%R) .NE. 4 )                 STOP 56
  IF ( ANY(A%R  .NE. (/(I, I=1,4)/)))     STOP 57
  IF ( KIND(A%Z) .NE. 4 )                 STOP 58
  IF ( SIZE(A%Z) .NE. 4 )                 STOP 59
  IF ( ANY(A%Z  .NE. (/((I,-I),I=1,4)/))) STOP 60
  IF ( LEN(A%C) .NE. 3 )                  STOP 61
  IF ( SIZE(A%C).NE. 4 )                  STOP 62
  IF ( ANY(A%C  .NE. CHAR(4)//" " ))      STOP 63
  IF ( ASSOCIATED(A%ProcPtr) )            STOP 64
  DEALLOCATE( A )

  END

