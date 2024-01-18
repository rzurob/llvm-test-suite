!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpNamelist3
!*
!*  DATE                       : Jul 07, 2007
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
!*  Namelist-group-name appear more than once in the NAMELIST statements in a scoping unit.
!*
!*  (ICE)
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
    LOGICAL(K1)   :: L(L2)=.FALSE.
    CHARACTER(L2) :: C(L2)=CHAR(48+K2)
    COMPLEX(K2)   :: Z(L2)=(-K2,K2)
    INTEGER(K2)   :: I(L2)=K2
  END TYPE
  SAVE

  TYPE(DT0(1,3))        :: R
  TYPE(DT1(1,3,4,5))    :: S
  TYPE(DT2(1,3,4,5,8,7)):: T

  NAMELIST /NL0/ R,S
  NAMELIST /NL0/ R,T

  NAMELIST /NL1/ S
  NAMELIST /NL1/ R,T,R

  NAMELIST /NL2/ R
  NAMELIST /NL2/ R

  NAMELIST /NL3/ R,R
  NAMELIST /NL3/ T,T
  NAMELIST /NL3/ R,R
  NAMELIST /NL3/ S,S

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpNamelist3
  USE M, ONLY:R,S,T

  INTERFACE
    SUBROUTINE S1()
    IMPORT R,S,T
      NAMELIST /NL0/ R,S
      NAMELIST /NL0/ R,T
      NAMELIST /NL1/ S
      NAMELIST /NL1/ R,T,R
      NAMELIST /NL2/ R
      NAMELIST /NL2/ R
      NAMELIST /NL3/ R,R
      NAMELIST /NL3/ T,T
      NAMELIST /NL3/ R,R
      NAMELIST /NL3/ S,S
    END SUBROUTINE
  END INTERFACE

  NAMELIST /NL0/ R,S
  NAMELIST /NL0/ R,T

  NAMELIST /NL1/ S
  NAMELIST /NL1/ R,T,R

  NAMELIST /NL2/ R
  NAMELIST /NL2/ R

  NAMELIST /NL3/ R,R
  NAMELIST /NL3/ T,T
  NAMELIST /NL3/ R,R
  NAMELIST /NL3/ S,S

  WRITE(*, NML=NL0)
  WRITE(*, NML=NL1)
  WRITE(*, NML=NL2)
  WRITE(*, NML=NL3)

  END


