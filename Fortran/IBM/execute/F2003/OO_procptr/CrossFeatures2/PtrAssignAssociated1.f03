! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 18, 2005
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
!*  The procedure pointer's status, proc ptr in common
!*  (315172/315269)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    CHARACTER(4), POINTER :: C
  END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg(:)
    CLASS(*), POINTER :: ModFun(:,:)
    INTEGER           :: I
      I = SQRT(1.*SIZE(Arg, 1))
      ALLOCATE(ModFun(I, I), SOURCE=RESHAPE(Arg, (/I,I/)))
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated1
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun), POINTER :: ProcPtr1
  PROCEDURE(ModFun), POINTER :: ProcPtr2
  CHARACTER(4),      POINTER :: CPtr
  COMMON /CB/ProcPtr1, CPtr, ProcPtr2

  ProcPtr1 => ModFun
  CALL IntSub(ProcPtr1)

  CONTAINS

  SUBROUTINE IntSub(ProcPtr)
  PROCEDURE(ProcPtr1), POINTER :: ProcPtr

  INTERFACE
    SUBROUTINE ExtSub(Proc)
      import
      PROCEDURE(ModFun)          :: Proc
    END SUBROUTINE
  END INTERFACE

  ProcPtr2 => ProcPtr
  ALLOCATE(CPtr, SOURCE="4321")

  CALL ExtSub(ProcPtr)

  END SUBROUTINE

  END

  SUBROUTINE ExtSub(Proc)
  USE M
  IMPLICIT NONE
  PROCEDURE(ModFun)          :: Proc
  PROCEDURE(ModFun), POINTER :: ProcPtr1
  PROCEDURE(ModFun), POINTER :: ProcPtr2
  CHARACTER(4),      POINTER :: CPtr
  INTEGER                    :: i
  COMMON /CB/ProcPtr1, CPtr, ProcPtr2

  IF ( .NOT. ASSOCIATED(CPtr) )  ERROR STOP 11
  IF ( CPtr .NE. "4321" )        ERROR STOP 12

  IF ( .NOT. ASSOCIATED(ProcPtr1) )            ERROR STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr1, ModFun) )    ERROR STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr1, Proc) )      ERROR STOP 23
  IF ( .NOT. ASSOCIATED(ProcPtr1, ProcPtr2) )  ERROR STOP 23

  SELECT TYPE ( As => RESHAPE(ProcPtr1((/( CPtr, i=1,256)/)), (/16,16 /) ))
  TYPE IS (CHARACTER(*))
    IF ( ANY( SHAPE(As) .NE. (/16,16/)))    ERROR STOP 31
    IF ( ANY( As        .NE. "4321" )  )    ERROR STOP 32
  CLASS DEFAULT
    STOP 33
  END SELECT


  END SUBROUTINE
