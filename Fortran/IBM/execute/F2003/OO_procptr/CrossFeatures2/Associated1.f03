! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 28, 2005
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
!*  ASSOCIATED(POINTER [, TARGET])
!*  Function return
!*  (314850)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    LOGICAL :: LSub = .FALSE.

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

    SUBROUTINE ModSub(Arg)
    CLASS(*) :: Arg
      LSub = .TRUE.
    END SUBROUTINE

  END MODULE

  FUNCTION ExtFun(Arg)
  CLASS(*), ALLOCATABLE :: ExtFun
  CLASS(*)              :: Arg
    ALLOCATE(ExtFun, SOURCE=Arg)
  END FUNCTION


  PROGRAM Associated1
  USE M
  IMPLICIT NONE

  INTERFACE ExtFun
    FUNCTION ExtFun(Arg)
      CLASS(*), ALLOCATABLE :: ExtFun
      CLASS(*)              :: Arg
    END FUNCTION
  END INTERFACE


  CALL IntSub(ModSub, IntFun1(ModFun) )

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcPtr)
  PROCEDURE(ModSub)          :: Proc
  PROCEDURE(ModFun), POINTER :: ProcPtr

  PROCEDURE(ModFun),  POINTER :: ProcPtr1 => NULL()
  PROCEDURE(ExtFun),  POINTER :: ProcPtr2 => NULL()
  PROCEDURE(ModSub),  POINTER :: ProcPtr3 => NULL()

  IF ( ASSOCIATED( ProcPtr1 ))              ERROR STOP 11
  ProcPtr1 => IntFun1(ProcPtr)
  IF ( .NOT. ASSOCIATED(ProcPtr1, ModFun )) ERROR STOP 12
  SELECT TYPE (As => ProcPtr1(100_1) )
  TYPE IS (INTEGER(1))
    IF ( As .NE. 100_1 )                    ERROR STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT

  IF ( ASSOCIATED( ProcPtr2 ))              ERROR STOP 21
  ProcPtr2 => IntFun2(ExtFun)
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun )) ERROR STOP 22
  SELECT TYPE (As => ProcPtr1((1.,-1.)) )
  TYPE IS (COMPLEX)
    IF ( As .NE. (1.0,-1.0) )               ERROR STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT

  IF ( ASSOCIATED( ProcPtr3 ))              ERROR STOP 31
  ProcPtr3 => IntFun3(Proc)
  IF ( .NOT. ASSOCIATED(ProcPtr3, ModSub )) ERROR STOP 32
  CALL ProcPtr3("12345")
  IF (  .NOT. LSub )                        ERROR STOP 33


  END SUBROUTINE

  FUNCTION IntFun1(Arg)
  PROCEDURE(ModFun)   :: Arg
  PROCEDURE(ModFun), POINTER :: IntFun1
    IntFun1 => Arg
  END FUNCTION

  FUNCTION IntFun2(Arg)
  PROCEDURE(ExtFun)   :: Arg
  PROCEDURE(ExtFun), POINTER :: IntFun2
    IntFun2 => Arg
  END FUNCTION

  FUNCTION IntFun3(Arg)
  PROCEDURE(ModSub)   :: Arg
  PROCEDURE(ModSub), POINTER :: IntFun3
    IntFun3 => Arg
  END FUNCTION


  END
