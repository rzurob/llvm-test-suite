!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr11
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 28, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
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
!*  The external attribute -- dummmy procedures 
!* 
!*
!* 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: L1
    CHARACTER(L1+3) :: C1 = "DT1" 
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2
    INTEGER(K1), LEN     :: L2
    CHARACTER(L2)        :: C2=""
    INTEGER(K2)  :: I
    REAL   (K2) :: R
    LOGICAL(K2)  :: L
    COMPLEX(K2) :: Z
    TYPE(DT0(K2, L2))    :: T0 
    TYPE(DT2(K2, L2,k2,k2, k2,k2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun 
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(Proc)
  !INTEGER :: L
  TYPE(DT2(2,2,2,2,8,8)) :: ModFun, Proc 
  EXTERNAL Proc 
    ModFun = Proc() 
    IF ( C  .NE.   "ExtFun"   ) STOP 33
    C = "ModFun"
  END FUNCTION

  END MODULE

  FUNCTION ExtFun()
  USE M
  !INTEGER :: L
  TYPE(DT2(2,2,2,2,8,8)) :: ExtFun
    ExtFun%C1 = "XX" 
    ExtFun%I = -ExtFun%I%KIND
    ExtFun%R = -ExtFun%R%KIND
    ExtFun%L = .TRUE. 
    ExtFun%Z = -(8., 8.) 
    ExtFun%T0 = DT0(8,8)()
    ExtFun%Ptr2 => NULL() 
    C = "ExtFun"
  END FUNCTION

  PROGRAM dtpObjDecAttr11
  USE M

  EXTERNAL ExtFun 
  TYPE(DT2(2,2,2,2,8,8)) :: T, ExtFun
  INTEGER  :: L=2
 
  T = T%Proc(ExtFun) 

  IF ( T%C1          .NE.   "XX"     ) STOP 11
  IF ( T%I           .NE.  -8        ) STOP 12
  IF ( T%R           .NE.  -8        ) STOP 13
  IF ( .NOT. T%L                     ) STOP 14
  IF ( T%Z           .NE.  -(8., 8.) ) STOP 15
  IF ( T%T0%K0       .NE.   8        ) STOP 16
  IF ( T%T0%L0       .NE.   8        ) STOP 17
  IF ( ASSOCIATED( T%Ptr2 )          ) STOP 18
  IF ( C  .NE.   "ModFun"            ) STOP 19


  END



