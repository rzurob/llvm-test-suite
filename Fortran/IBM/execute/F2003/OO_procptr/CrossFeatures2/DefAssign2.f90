! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 23, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Defined assignment - where
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


    TYPE :: Base
      INTEGER :: Id=0
    END TYPE

    TYPE, EXTENDS(Base) :: DT
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc => Modfun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT) :: Arg
    TYPE(DT) :: ModFun
      ModFun = Arg
    END FUNCTION

    ELEMENTAL SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1
    TYPE(Base), INTENT(IN)  :: Arg2
      Arg1 = Arg2
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign2 (Arg1, arg2)
    TYPE(DT),    INTENT(OUT) :: Arg1
    TYPE(Base),  INTENT(IN)  :: Arg2
      Arg1%Base = Arg2
      Arg1%ProcPtr => ModFun
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign3 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1
    TYPE(DT),   INTENT(IN)  :: Arg2
      Arg1 = Arg2%Base
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign4 (Arg1, Arg2)
    TYPE(DT),  INTENT(OUT) :: Arg1
    TYPE(DT),  INTENT(IN)  :: Arg2
      Arg1%ProcPtr => Arg2%ProcPtr
      Arg1%Id = Arg2%Id
    END SUBROUTINE


  END MODULE


  PROGRAM DefAssign2
  USE M
  IMPLICIT NONE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
      MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
      MODULE PROCEDURE MyAssign4
    END INTERFACE ASSIGNMENT ( = )

  INTEGER :: I
  TYPE(Base) :: B1(511), B2(511)
  TYPE(DT)   :: D1(511), D2(511)

  WHERE ((/(.TRUE., I=1,511)/)  )
    B1 = Base(1)
  END WHERE
  DO I=1, 511
    IF (B1(I)%Id .NE. 1) ERROR STOP 11
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D1 = Base(2)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D1(I)%ProcPtr, ModFun)) ERROR STOP 22
    IF (D1(I)%Id .NE. 2) ERROR STOP 23
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    B2 = DT(-1, ModFun)
  END WHERE
  DO I=1, 511
   IF (B2(I)%Id .NE. -1) ERROR STOP 33
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D2 = DT(-2, ModFun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D2(I)%ProcPtr, ModFun)) ERROR STOP 42
    IF (D2(I)%Id .NE. -2) ERROR STOP 43
  END DO


  END

