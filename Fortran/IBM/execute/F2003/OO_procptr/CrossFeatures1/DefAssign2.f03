! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2005
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToC
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id=0
      TYPE(Base), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
      MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
      MODULE PROCEDURE MyAssign4
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    ELEMENTAL SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1
    TYPE(Base), INTENT(IN)  :: Arg2
      Arg1%ProcPtr => Arg2%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign2 (Arg1, arg2)
    TYPE(DT),    INTENT(OUT) :: Arg1
    TYPE(Base),  INTENT(IN)  :: Arg2
      Arg1%Id = -1
      ALLOCATE(Arg1%BComp)
      Arg1%BComp%ProcPtr => Arg2%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign3 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1
    TYPE(DT),   INTENT(IN)  :: Arg2
      Arg1%ProcPtr => Arg2%BComp%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign4 (Arg1, Arg2)
    TYPE(DT),  INTENT(OUT) :: Arg1
    TYPE(DT),  INTENT(IN)  :: Arg2
      ALLOCATE(Arg1%BComp)
      Arg1%BComp%ProcPtr => Arg2%BComp%ProcPtr
      Arg1%Id = -Arg2%Id
    END SUBROUTINE


  END MODULE


  PROGRAM DefAssign2
  USE M
  IMPLICIT NONE

  INTEGER :: I
  TYPE(Base) :: B1(511), B2(511)
  TYPE(DT)   :: D1(511), D2(511)
  TYPE(Base), TARGET :: BTar

  WHERE ((/(.TRUE., I=1,511)/)  )
    B1 = Base(Fun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(B1(I)%ProcPtr, Fun)) ERROR STOP 11
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D1 = Base(Fun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D1(I)%BComp%ProcPtr, Fun)) ERROR STOP 22
    IF (D1(I)%Id .NE. -1) ERROR STOP 23
  END DO

  BTar = Base(RetPtr(Fun))
  WHERE ((/(.TRUE., I=1,511)/)  )
    B2 = DT(-1, BTar)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(B2(I)%ProcPtr, Fun)) ERROR STOP 32
  END DO

  BTar = Base(Fun)
  WHERE ((/(.TRUE., I=1,511)/)  )
    D2 = DT(-3, BTar)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D2(I)%BComp%ProcPtr, Fun)) ERROR STOP 42
    IF (D2(I)%Id .NE. 3) ERROR STOP 43
  END DO

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

