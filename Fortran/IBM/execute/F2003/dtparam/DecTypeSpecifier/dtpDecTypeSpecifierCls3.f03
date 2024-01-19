!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 18, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
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
!*  The dynamic type of an unallocated allocatable or a disassociated pointer is
!*  the same as its declared type
!*
!*  -- The dynamic type
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpDecTypeSpecifierCls3

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=0
    INTEGER, LEN  :: L0=0
  END TYPE

  TYPE, EXTENDS(DT0)  :: DT1(K1,L1)
    INTEGER(K0), KIND :: K1=2*K0
    INTEGER(K0), LEN  :: L1=2*K0
    INTEGER(K1)       :: I=K1
    CHARACTER(L0)     :: C(L1)="X"
  END TYPE


  CLASS(DT0), ALLOCATABLE :: T1
  CLASS(DT0(2,2)), ALLOCATABLE :: T2
  CLASS(DT1(1,1,K1=2,L1=2)), ALLOCATABLE :: T3

  CLASS(DT0), POINTER :: P1 => NULL()
  CLASS(DT0(2,2)), POINTER :: P2 => NULL()
  CLASS(DT1(1,1,K1=2,L1=2)), POINTER :: P3 => NULL()


  IF ( .NOT. SAME_TYPE_AS(T1, DT0()) )              ERROR STOP 11

  IF ( .NOT. SAME_TYPE_AS(T2, DT0(2,2)() ) )        ERROR STOP 12

  IF ( .NOT. SAME_TYPE_AS(T3, DT1(1,1,2,2)(I=-1)) ) ERROR STOP 13


  IF ( .NOT. SAME_TYPE_AS(P1, DT0()) )              ERROR STOP 21

  IF ( .NOT. SAME_TYPE_AS(P2, DT0(2,2)()) )         ERROR STOP 22

  IF ( .NOT. SAME_TYPE_AS(P3, DT1(1,1,2,2)(I=-1)) ) ERROR STOP 23


  END

