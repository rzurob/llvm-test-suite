! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/DefAssign2.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: DefAssign2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DefAssign2.f
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


    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id=0
    END TYPE

    TYPE, EXTENDS(Base) :: DT    ! (4)
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc => Modfun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(4)) :: Arg
    TYPE(DT(4)) :: ModFun
      ModFun = Arg
    END FUNCTION

    ELEMENTAL SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(Base(4)), INTENT(OUT) :: Arg1
    TYPE(Base(4)), INTENT(IN)  :: Arg2
      Arg1 = Arg2
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign2 (Arg1, arg2)
    TYPE(DT(4)),    INTENT(OUT) :: Arg1
    TYPE(Base(4)),  INTENT(IN)  :: Arg2
      Arg1%Base = Arg2
      Arg1%ProcPtr => ModFun
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign3 (Arg1, Arg2)
    TYPE(Base(4)), INTENT(OUT) :: Arg1
    TYPE(DT(4)),   INTENT(IN)  :: Arg2
      Arg1 = Arg2%Base
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign4 (Arg1, Arg2)
    TYPE(DT(4)),  INTENT(OUT) :: Arg1
    TYPE(DT(4)),  INTENT(IN)  :: Arg2
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
  TYPE(Base(4)) :: B1(511), B2(511)
  TYPE(DT(4))   :: D1(511), D2(511)

  WHERE ((/(.TRUE., I=1,511)/)  )
    B1 = Base(4)(1)
  END WHERE
  DO I=1, 511
    IF (B1(I)%Id .NE. 1) STOP 11
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D1 = Base(4)(2)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D1(I)%ProcPtr, ModFun)) STOP 22
    IF (D1(I)%Id .NE. 2) STOP 23
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    B2 = DT(4)(-1, ModFun)
  END WHERE
  DO I=1, 511
   IF (B2(I)%Id .NE. -1) STOP 33
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D2 = DT(4)(-2, ModFun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D2(I)%ProcPtr, ModFun)) STOP 42
    IF (D2(I)%Id .NE. -2) STOP 43
  END DO


  END

