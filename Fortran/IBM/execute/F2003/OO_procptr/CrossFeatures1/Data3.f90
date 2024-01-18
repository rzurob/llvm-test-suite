! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Except for variables in named common blocks, a named variable has the SAVE
!*  attribute if any part of it is initialized in a DATA statement.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun),NOPASS,  POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data3
  USE M
  IMPLICIT NONE

  CALL IntSub(0)
  CALL IntSub(1)

  CONTAINS

  SUBROUTINE IntSub(Arg)
  INTEGER :: Arg

  TYPE (DT) :: V
  DATA V /DT(-1, NULL())/

  PROCEDURE(Fun), POINTER :: ProcPtr
  DATA ProcPtr /NULL()/

  SELECT CASE (Arg)
  CASE(0)

    IF (ASSOCIATED(V%ProcPtr))     STOP 11
    IF (V%Id .NE. -1)              STOP 12

    V%ProcPtr => Fun
    V%Id =  1

  CASE(1)

    IF (.NOT. ASSOCIATED(V%ProcPtr, Fun))  STOP 21
    IF (V%Id .NE. 1)                       STOP 22

  END SELECT
  END SUBROUTINE

  END


