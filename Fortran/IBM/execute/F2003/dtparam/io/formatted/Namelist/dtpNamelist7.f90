!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpNamelist7
!*
!*  DATE                       : Jun. 29, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
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
!*  -- The namelist statement
!*  Implicit typing rules
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE :: ModFun
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    REAL(K1) :: R(L1)=K1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    CHARACTER(L2) :: C(L2)=CHAR(48+K2)
    INTEGER(K2)   :: I(L2)=K2
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpNamelist7
  USE M

  implicit type(dt0(1,:)) (r)
  IMPLICIT TYPE(DT1(1,:,4,:))(S)
  IMPLICIT TYPE(DT2(1,:,4,:,8,:))(T)

  ALLOCATABLE :: R(:)
  ALLOCATABLE :: S(:)
  ALLOCATABLE :: T(:)

  ALLOCATE(DT0(1,3)         :: R(10:10))
  ALLOCATE(DT1(1,3,4,5)     :: S(10:10))
  ALLOCATE(DT2(1,3,4,5,8,7) :: T(10:10))

  CALL IntSub(R,S,T,1)

  CONTAINS

  SUBROUTINE IntSub(R,S,T,N)
  USE M, ONLY: DT0,DT1,DT2
  TYPE(DT0(1,*)) R(:)
  TYPE(DT1(1,*,4,*)) S(:)
  TYPE(DT2(1,*,4,*,8,*)) T(:)

  INTEGER N

  NAMELIST /NL0/R, /NL1/S /NL2/T

  WRITE(*, NML=NL0)
  WRITE(*, NML=NL1)
  WRITE(*, NML=NL2)

  END SUBROUTINE

  END


