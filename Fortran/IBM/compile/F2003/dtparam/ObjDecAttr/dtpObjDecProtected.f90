!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecCProtected
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 04, 2007
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
!*  -- Protected 
!* 
!*
!*     
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1"
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    INTEGER(K2)          :: I=K2
    TYPE(DT0(K2, L2))           :: T0
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr2
  END TYPE

  TYPE(DT0),              PROTECTED              :: T1
  CLASS(DT1(2,:,4,:)),    PROTECTED, POINTER     :: T2
  TYPE(DT2(2,3,4,5,8,7)), PROTECTED, SAVE        :: T3
  TYPE(DT2(2,3,4,:,8,7)), PROTECTED, ALLOCATABLE :: T4(:)

  END MODULE

  PROGRAM dtpObjDecCProtected
  USE M

  T1 = DT()

  T2%C1 = ""

  T3%Ptr2 => NULL()

  T3%T0 = DT0()
 
  T3%I = 0

  T4(1)%Ptr2 => NULL()  

  T4(1)%I = 0  
 
  END

