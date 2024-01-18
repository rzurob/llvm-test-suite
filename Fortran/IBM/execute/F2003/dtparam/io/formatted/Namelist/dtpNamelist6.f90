!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpNamelist6
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 29, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
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
!* 
!*  -- The namelist statement
!*  Namelist group objects are dummy 
!* 
!*  (ice)
!*   
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
  SAVE
 
  TYPE(DT0(1,:))        , ALLOCATABLE :: R(:)
  TYPE(DT1(1,:,4,:))    , ALLOCATABLE :: S(:)
  TYPE(DT2(1,:,4,:,8,:)), ALLOCATABLE :: T(:)

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpNamelist6
  USE M

  ALLOCATE(DT0(1,3)         :: R(1))
  ALLOCATE(DT1(1,3,4,5)     :: S(1))
  ALLOCATE(DT2(1,3,4,5,8,7) :: T(1))

  CALL IntSub([R],[S],[T],1)

  CONTAINS
 
  SUBROUTINE IntSub(R,S,T,N)
  USE M, ONLY: DT0,DT1,DT2
  INTEGER N
  TYPE(DT0(1,*))        :: R(-N:-1)
  TYPE(DT1(1,*,4,*))    :: S(-N:-1)
  TYPE(DT2(1,*,4,*,8,*)):: T(-N:-1)

  NAMELIST /NL0/ R, /NL1/S /NL2/T

  WRITE(*, NML=NL0)
  WRITE(*, NML=NL1) 
  WRITE(*, NML=NL2)
  
  END SUBROUTINE

  END


