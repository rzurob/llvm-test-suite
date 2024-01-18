!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommon11 
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
!*    -- Procedure pointers with an Explicit interface on array
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
    INTEGER(K)     :: I(L2)
    CHARACTER(L2)  :: C2(L1)
  END TYPE

  PROCEDURE(F), POINTER  :: ProcPtr0
  COMMON ProcPtr0

  CONTAINS

  FUNCTION F(N, C1, I, C2)
  INTEGER      :: N, I(:)
  CHARACTER(*) :: C1(:)
  CHARACTER(*) :: C2(:)
  TYPE(DT(2,C1%LEN,C2%LEN)) :: F(N)
    F = DT(2,C1%LEN, C2%LEN)(C1, I, C2) 
  END FUNCTION

  END MODULE


  PROGRAM dtpCommon11 
  USE M
  IMPLICIT NONE

  PROCEDURE(F), POINTER  :: ProcPtr

  TYPE(DT(2,7,9)) :: T(1024)
  CHARACTER(7)    :: C1(9)
  CHARACTER(9)    :: C2(7)
  INTEGER         :: K(9) 
  INTEGER         :: I
  COMMON ProcPtr,T,C1,K,I,C2

  ProcPtr0 => F
  C1 = "123456789"
  K=[1,2,3,4,5,6,7,8,9]
  C2 = "7654321"
 
  T = ProcPtr(1024, C1, K, C2) 

  IF ( .NOT. ASSOCIATED(ProcPtr, F) ) STOP 10
  DO I=1, 1024
    IF (  ANY ( T(I)%C1 .NE. C1    ) ) STOP 11
    IF (  ANY ( T(I)%I  .NE. K     ) ) STOP 12
    IF (  ANY ( T(I)%C2 .NE. C2    ) ) STOP 13
  END DO

  END PROGRAM

