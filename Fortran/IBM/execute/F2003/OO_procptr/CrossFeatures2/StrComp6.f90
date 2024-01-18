! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 24, 2005
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
!*  on pure function
!*  (314926)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1
    END TYPE

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2
      TYPE(Base), ALLOCATABLE :: BComp
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun3
    END TYPE

    CONTAINS

    PURE FUNCTION ModFun1(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    TYPE (Base) :: ModFun1
      ModFun1%ProcPtr1 => Arg%ProcPtr1
    END FUNCTION

    PURE FUNCTION ModFun2(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    TYPE (DT) :: ModFun2
      ModFun2%ProcPtr1 => Arg%ProcPtr1
      ModFun2%ProcPtr2 => Arg%ProcPtr2
      ALLOCATE(ModFun2%BComp)
      ModFun2%BComp%ProcPtr1 => Arg%ProcPtr1
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg3)
    CLASS(DT) :: Arg1
    PROCEDURE(ModFun1)          :: Arg3
    PROCEDURE(ModFun1), POINTER :: ModFun3
      ModFun3 => Arg3
    END FUNCTION

  END MODULE

  PROGRAM StrComp6
  USE M
  TYPE(DT),   POINTER :: V
  TYPE(Base), POINTER :: V1
  TYPE(DT), POINTER :: U(:)
  TYPE(DT)          :: U1(513)
  PROCEDURE(ModFun1), POINTER :: ProcPtr

  ALLOCATE(V, SOURCE=DT(Base=Base(ModFun1), BComp=Base(ModFun1), ProcPTr2=Modfun2))

  IF ( .NOT. ASSOCIATED(V%ProcPtr1, Modfun1)) STOP 11
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, Modfun2)) STOP 12
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr1, Modfun1)) STOP 13

  ALLOCATE(V1)
  V1%ProcPtr1 => Null()
  V1 = V%Base
  IF ( .NOT. ASSOCIATED(V1%ProcPtr1, Modfun1)) STOP 14

  V1%ProcPtr1 => Null()
  V1 = V%BComp
  IF ( .NOT. ASSOCIATED(V1%ProcPtr1, Modfun1)) STOP 15

  V1 = V%ProcPtr1()
  IF ( .NOT. ASSOCIATED(V1%ProcPtr1, Modfun1)) STOP 16

  U1 = DT(Base=Base(ModFun1), BComp=Base(ModFun1), ProcPTr2=Modfun2)
  ALLOCATE(U(513), SOURCE=U1)

  DO I=1,513
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Modfun1)) STOP 17
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr2, Modfun2)) STOP 18
    IF ( .NOT. ASSOCIATED(U(I)%BComp%ProcPtr1, Modfun1)) STOP 19
  END DO

  ProcPtr => U(1)%ProcPtr(ModFun1)
  IF ( .NOT. ASSOCIATED(ProcPtr, Modfun1)) STOP 21

  END

