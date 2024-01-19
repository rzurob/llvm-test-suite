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
!*  (304454)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS
    FUNCTION ModFun(Arg)
    CHARACTER(4)          :: Arg(:)
    CHARACTER(4), POINTER :: ModFun(:)
      !ALLOCATE(ModFun(SIZE(Arg)), SOURCE=Arg)  ! not 10.1
      ALLOCATE(ModFun(SIZE(Arg)))
      ModFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated1
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun), POINTER :: ProcPtr
  CHARACTER(4),      POINTER :: CPtr

  INTERFACE
    SUBROUTINE ExtSub(Proc)
      IMPORT
      PROCEDURE(ModFun) :: Proc
    END SUBROUTINE
  END INTERFACE

  COMMON /CB/ProcPtr, CPtr

  !ALLOCATE(CPtr, SOURCE="4321") ! not 10.1
  ALLOCATE(CPtr)
  CPtr = "4321"

  ProcPtr => ModFun
  CALL ExtSub(ProcPtr)

  END

  SUBROUTINE ExtSub(Proc)
  USE M
  IMPLICIT NONE
  PROCEDURE(ModFun)          :: Proc
  PROCEDURE(ModFun), POINTER :: ProcPtr
  CHARACTER(4),      POINTER :: CPtr
  INTEGER :: i
  COMMON /CB/ProcPtr, CPtr

  IF ( .NOT. ASSOCIATED(CPtr) )  ERROR STOP 11
  IF ( CPtr .NE. "4321" )        ERROR STOP 12

  IF ( .NOT. ASSOCIATED(ProcPtr) )          ERROR STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) )  ERROR STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr, Proc) )    ERROR STOP 23

  ASSOCIATE ( As => RESHAPE(ProcPtr((/( "IBM!", i=1,256)/)), (/16,16 /) ))
    IF ( ANY( SHAPE(As) .NE. (/16,16/) ))   ERROR STOP 31
    IF ( ANY( As        .NE. "IBM!" ))      ERROR STOP 32
  END ASSOCIATE

  END SUBROUTINE

