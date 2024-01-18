!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpNamelist2
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
!*  The order in which the variables are specified in the NAMELIST statement determines
!*  the order in which the values appear on output.
!* 
!*  (ICE)
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
    LOGICAL(K1)   :: L(L2)=.FALSE.
    CHARACTER(L2) :: C(L2)=CHAR(48+K2)
    COMPLEX(K2)   :: Z(L2)=(-K2,K2)
    INTEGER(K2)   :: I(L2)=K2
  END TYPE
  SAVE
 
  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpNamelist2
  USE M

  TYPE(DT0(1,3))        :: R
  TYPE(DT1(1,3,4,5))    :: S
  TYPE(DT2(1,3,4,5,8,7)):: T

  CALL IntSub([R,R,R],[S,S,S],[T,T,T],3)

  CONTAINS
 
  SUBROUTINE IntSub(r,s,t,N)
  INTEGER N
  TYPE(DT0(1,*))        :: R(N)
  TYPE(DT1(1,*,4,*))    :: S(N)
  TYPE(DT2(1,*,4,*,8,*)):: T(N)

  NAMELIST /NL0/ R,S,T
  NAMELIST /NL1/ S,R,T
  NAMELIST /NL2/ S,T,R
  NAMELIST /NL3/ T,R,S

  WRITE(*, NML=NL0)
  WRITE(*, NML=NL1) 
  WRITE(*, NML=NL2)
  WRITE(*, NML=NL3)
  
  END SUBROUTINE

  END


