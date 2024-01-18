! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures2/Arg6.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2005
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
!*  Argument association - Implicit interface  of subroutine
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT    ! (4,3)
      PROCEDURE(), NOPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NOPASS :: Proc=>ModSub
    END TYPE

    CONTAINS

    SUBROUTINE ModSub(Arg1, Arg2)
    TYPE(DT(4,*)), INTENT(IN)  :: Arg2
    TYPE(DT(4,*)), INTENT(OUT) :: Arg1
      Arg1 = Arg2
    END SUBROUTINE

    SUBROUTINE ISub(Arg1, Arg2)
    TYPE(DT(4,*)), INTENT(IN)  :: Arg2
    TYPE(DT(4,*)), INTENT(OUT) :: Arg1
      Arg1 = Arg2
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT(4,*)), INTENT(IN)  :: Arg2
  TYPE(DT(4,*)), INTENT(OUT) :: Arg1
    Arg1 = Arg2
  END SUBROUTINE

  PROGRAM Arg6
  USE M
  IMPLICIT NONE
  PROCEDURE(Isub) :: ExtSub
  PROCEDURE(ISub), POINTER :: ProcPtr

  ProcPtr => ExtSub
  CALL IntSub( ProcPtr, ProcPtr )

  CALL IntSub( IntFun1(ExtSub), IntFun1(ProcPtr))
  CALL IntSub( ProcPtr, IntFun2(ExtSub))

  CONTAINS

  FUNCTION IntFun1(Proc)
  PROCEDURE(ISub) :: Proc
  PROCEDURE(ISub), POINTER :: IntFun1
    IntFun1 => Proc
  END FUNCTION

  FUNCTION IntFun2(Proc)
  PROCEDURE(ISub) :: Proc
  PROCEDURE(ISub), POINTER :: IntFun2
    IntFun2 => Proc
  END FUNCTION


  SUBROUTINE IntSub(ProcPtr0, ProcPtr1)
  IMPLICIT TYPE(Base(4,3))(P)

  PROCEDURE(ISub), POINTER :: ProcPtr0
  PROCEDURE(ISub), POINTER :: ProcPtr1
  TYPE(DT(4,3))    :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtSub) ) ERROR STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtSub) ) ERROR STOP 11

  CALL ProcPtr0(V, DT(4,3)("321", ProcPtr1))
  IF (V%C .NE. "321") ERROR STOP 15
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtSub)) ERROR STOP 16

  CALL ProcPtr1(V, DT(4,3)("321", ProcPtr1))
  IF (V%C .NE. "321") ERROR STOP 17
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtSub)) ERROR STOP 18

  END SUBROUTINE

  END

