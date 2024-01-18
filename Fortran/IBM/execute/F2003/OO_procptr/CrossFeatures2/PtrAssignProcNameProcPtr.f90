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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  The target is a procedure target
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    INTERFACE
      FUNCTION IFun(Arg)
        CLASS(*)          :: Arg
        CLASS(*), POINTER :: IFun
      END FUNCTION

      FUNCTION IFun1(Arg)
        CLASS(*)          :: Arg(:)
        CLASS(*), POINTER :: IFun1(:)
      END FUNCTION
    END INTERFACE

  CONTAINS

  SUBROUTINE ModSub(ArgPtr)

    PROCEDURE(IFun), POINTER           :: Ptr
    PROCEDURE(IFun), POINTER, OPTIONAL :: ArgPtr

    Ptr => ArgPtr

    SELECT TYPE (As => Ptr("1234") )
    TYPE IS (CHARACTER(*))
      IF ( LEN(As) .NE. 4 )      STOP 11
      IF ( As      .NE. "1234" ) STOP 12
    CLASS DEFAULT
      STOP 14
    END SELECT

  END SUBROUTINE

  FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
  END FUNCTION

  FUNCTION ModFunArr(Arg)
    CLASS(*)          :: Arg(:)
    CLASS(*), POINTER :: ModFunArr(:)
      ALLOCATE(ModFunArr(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  END MODULE


  PROGRAM PtrAssignProcNameProcPtr
  USE M
  IMPLICIT NONE

  PROCEDURE(IFun),  POINTER :: Ptr1
  PROCEDURE(IFun1), POINTER :: Ptr2

  Ptr1 => ModFun
  CALL ModSub(Ptr1)

  Ptr2 => ModFunArr
  CALL IntSub(Ptr2)

  CONTAINS

  SUBROUTINE IntSub(ArgPtr)
  INTEGER                  :: i
  PROCEDURE(IFun1), POINTER :: Ptr
  PROCEDURE(IFun1), POINTER :: ArgPtr

    Ptr => ArgPtr
    SELECT TYPE ( As => Ptr((/((1.0_8, -1.0_8) , i=1, 511)/)) )
    TYPE IS (COMPLEX(8))
      IF (ANY(SHAPE(As) .NE. (/511/) ) )         STOP 21
      IF (ANY(As        .NE. (1.0_8, -1.0_8) ) ) STOP 22
    CLASS DEFAULT
      STOP 23
    END SELECT

  END SUBROUTINE

  END

