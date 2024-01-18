! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Assign4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Assign4.f
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
!*  Defined assignment-array
!*  (314836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc1 => ModFun1
    END TYPE

    TYPE, EXTENDS(BAse) :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc2 => ModFun2
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(Base) :: Arg
    CLASS(Base), POINTER :: ModFun1(:)
      ALLOCATE(ModFun1(3), SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT) :: Arg
    CLASS(DT), POINTER :: ModFun2(:)
      ALLOCATE(ModFun2(3), SOURCE=Arg)
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT), INTENT (OUT) :: Arg1(:)
    TYPE(Dt), INTENT (IN)  :: Arg2(SIZE(Arg1))
      DO I=1, SIZE(Arg1)
        Arg1(I)%Base = Arg2(I)%Base
        Arg1(I)%ProcPtr2 =>  Arg2(I)%ProcPtr2
      END DO
    END SUBROUTINE

  END MODULE


  PROGRAM Assign4
  USE M
  IMPLICIT NONE

  TYPE (DT)              :: V(1023)
  TYPE (Base)            :: V1(1023)
  TYPE (DT), ALLOCATABLE :: V2(:)
  TYPE (Base), POINTER   :: V3(:)
  INTEGER                :: I

  PROCEDURE(ModFun1), POINTER :: ProcPtr1
  PROCEDURE(ModFun2), POINTER :: ProcPtr2

  ProcPtr1 => ModFun1
  ProcPtr2 => ModFun2

  V = DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )
  V1 = Base(ModFun1)
  ALLOCATE(V2(1023), SOURCE=DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 ))
  ALLOCATE(V3(1023), SOURCE=Base(ProcPtr1) )

  DO I=1, 1023
    IF (.NOT. Equal(V(I),  DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 11
    IF (.NOT. Equal(V1(I), Base(ProcPtr1)) )                             STOP 12
    IF (.NOT. Equal(V2(I), DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 13
    IF (.NOT. Equal(V3(I), Base(ProcPtr1)) )                             STOP 14
  END DO

  CONTAINS

  FUNCTION Equal(Arg1, Arg2)
  LOGICAL Equal
  CLASS(Base) :: Arg1, Arg2

  Equal = .FALSE.

  SELECT TYPE ( Arg1 )
  TYPE IS (Base)
    SELECT TYPE ( Arg2 )
    TYPE IS (Base )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1)
    CLASS DEFAULT
      STOP 22
    END SELECT
  TYPE IS (DT)
    SELECT TYPE ( Arg2 )
    TYPE IS (DT )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1)
      Equal = Equal .AND. ASSOCIATED(Arg1%ProcPtr2, Arg2%ProcPtr2)
    CLASS DEFAULT
      STOP 33
    END SELECT
  CLASS DEFAULT
    STOP 44
  END SELECT

  END FUNCTION

  END

