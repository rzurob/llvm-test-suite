!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr12
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
!*  The external attribute -- procedure pointer 
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
    INTEGER, KIND :: K1=K0
    INTEGER, LEN  :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1" 
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=""
    INTEGER(K2), ALLOCATABLE  :: I
    REAL   (K2), ALLOCATABLE :: R
    LOGICAL(K2), ALLOCATABLE  :: L
    COMPLEX(K2), ALLOCATABLE :: Z
    TYPE(DT0(K2, L2)), ALLOCATABLE    :: T0 
    TYPE(DT2(K2, L2, k2, k2,k2,k2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun 
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(ProcPtr)
  INTEGER :: L
  TYPE(DT2(2,2,2,2,8,8)) :: ModFun
  procedure(type(DT2(2,2,2,2,8,8))), pointer :: ProcPtr 
    if (associated(ProcPtr)) then
        ModFun = ProcPtr()
    else
        stop 44
    end if
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

  PROGRAM dtpObjDecAttr12
  USE M

  EXTERNAL ExtFun 
  TYPE(DT2(2,2,2,2,8,8)) :: T, ExtFun
  INTEGER  :: L=2
  procedure(type(DT2(2,2,2,2,8,8))), pointer :: ProcPtr 

  ProcPtr => ExtFun 
  T = T%Proc(ProcPtr) 

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

