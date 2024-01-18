! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: StrComp10.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp10.f
!*
!*  DATE                       : May. 18, 2005
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
!*  Procedure pointer components
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    TYPE  :: DT
      TYPE(Base) :: BComp
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CHARACTER(3) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM StrComp10
  USE M
  IMPLICIT TYPE(DT)(P)

  INTEGER :: I
  PROCEDURE(CHARACTER(3)),  POINTER :: ProcPtr=>NULL()
  TYPE(DT) :: V(512)=(/(DT(Base("123",NULL())), I=1, 512)/)
  TYPE(DT) :: U(SIZE(V)), W(SIZE(V))

  DO I=1, 512
    IF ( ASSOCIATED(V(I)%BComp%ProcPtr)) STOP 11
  END DO

  ProcPtr => ModFun
  V = (/(DT(Base("123", ModFun)), I=1, 512)/)

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(V(I)%BComp%ProcPtr, ModFun)) STOP 21
  END DO

  U = V

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(U(I)%BComp%ProcPtr, ModFun)) STOP 21
  END DO

  WHERE ((/(.TRUE._2, i=1, 512)/))
    W = V
  END WHERE

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(W(I)%BComp%ProcPtr, ModFun)) STOP 21
  END DO

  END

