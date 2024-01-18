!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecC546
!*
!*  DATE                       : May. 29, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
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
!*  C546 (R517) A pointer object with the INTENT (IN) attribute shall not appear as
!* (1) A pointer-object in a nullify-stmt,
!* (2) A data-pointer-object or proc-pointer-object in a pointer-assignment-stmt,
!* (3) An allocate-object in an allocate-stmt or deallocate-stmt, or
!* (4) An actual argument in a reference to a procedure if the associated dummy argument is a
!*  pointer with the INTENT (OUT) or INTENT (INOUT) attribute.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpObjDecC546

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, KIND :: L=4
    CHARACTER(L)  :: C=CHAR(K)
  END TYPE

  CONTAINS

  SUBROUTINE IntSub(Arg)
  TYPE(DT(1,1)), INTENT(IN), POINTER :: Arg

  NULLIFY(Arg)
  Arg  => NULL()
  ALLOCATE(DT(1,1) :: Arg)
  CALL IntSub1(Arg)

  END SUBROUTINE

  SUBROUTINE IntSub1(Arg)
  TYPE(DT(1,1)), INTENT(INOUT), POINTER :: Arg
  END SUBROUTINE

  END

