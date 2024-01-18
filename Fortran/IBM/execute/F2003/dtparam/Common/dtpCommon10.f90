!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommon10 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 19, 2007
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
!*  -- The common statement
!* 
!*     A procedure pointer shall be storage associated only with another procedure pointer; 
!*     either both interfaces shall be explicit or both interfaces shall be implicit
!*   
!*    -- Procedure pointers with an explicit interface 
!* 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
 
  TYPE :: DT(K,L1,L2)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L1=4
    INTEGER, LEN  :: L2=4
    SEQUENCE
    CHARACTER(L1)  :: C1(L2)
    CHARACTER(L2)  :: C2(L1)
  END TYPE

  PROCEDURE(F), POINTER  :: ProcPtr0
  COMMON /MyBlk/ProcPtr0

  CONTAINS

  FUNCTION F(C1, C2)
  CHARACTER(*) :: C1(:)
  CHARACTER(*) :: C2(:)
  TYPE(DT(2,C1%LEN,C2%LEN)) :: F
    F = DT(2,C1%LEN, C2%LEN)(C1, C2) 
  END FUNCTION

  END MODULE


  PROGRAM dtpCommon10 
  USE M
  IMPLICIT NONE

  PROCEDURE(F), POINTER  :: ProcPtr
  COMMON /MyBlk/ProcPtr

  TYPE(DT(2,7,9)) :: T
  CHARACTER(7)    :: C1(9)
  CHARACTER(9)    :: C2(7)

  ProcPtr0 => F
  C1 = "123456789"
  C2 = "7654321"
 
  T = ProcPtr(C1, C2) 

  IF ( .NOT. ASSOCIATED(ProcPtr, F) ) STOP 10
  IF (  ANY ( T%C1 .NE. C1        ) ) STOP 11
  IF (  ANY ( T%C2 .NE. C2        ) ) STOP 12


  END PROGRAM

