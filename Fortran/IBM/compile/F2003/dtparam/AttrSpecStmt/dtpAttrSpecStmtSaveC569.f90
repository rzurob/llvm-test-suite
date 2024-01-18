!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtSaveC569
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2007
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
!*  -- C569 (R545) A proc-pointer-name shall be the name of a procedure pointer. 
!* 
!*
!*     
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpAttrSpecStmtSaveC569

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    INTEGER(K0)   :: I(L0)=K0
  END TYPE

  PROCEDURE(Func) Proc
  SAVE Proc

  PROCEDURE(Func), POINTER :: ProcPtr
  SAVE ProcPtr
 
  CONTAINS

  FUNCTION Func(Arg)
    TYPE(DT0(8,11)) :: Func, Arg
    Func = Arg
  END FUNCTION

  END

