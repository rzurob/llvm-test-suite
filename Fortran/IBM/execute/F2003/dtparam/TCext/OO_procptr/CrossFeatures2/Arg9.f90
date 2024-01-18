! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures2/Arg9.f
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
!*  Argument association - array
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

    TYPE, EXTENDS(Base)  :: DT    ! (4,3)
    CONTAINS
      PROCEDURE, PASS(PassObj) :: Proc=>ModFun
    END TYPE

    CONTAINS

    PURE FUNCTION ModFun(PassObj, Arg)
    CLASS(DT(4,*)),   INTENT(IN) :: PassObj
    CLASS(Base(4,*)), INTENT(IN) :: Arg(:)
    TYPE(DT(4,3))                :: ModFun(SIZE(Arg))
      ModFun=PassObj
    END FUNCTION

    PURE FUNCTION IFun(PassObj, Arg)
    CLASS(DT(4,*)),   INTENT(IN) :: PassObj
    CLASS(Base(4,*)), INTENT(IN) :: Arg(:)
    TYPE(DT(4,3))                :: IFun(SIZE(Arg))
      IFun=PassObj
    END FUNCTION

  END MODULE

  PROGRAM Arg9
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun), POINTER :: ProcPtr
  CALL IntSub1(ModFun )

  ProcPtr => ModFun
  CALL IntSub1( ProcPtr )
  CALL IntSub2( ProcPtr )

  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IFun) :: Arg
  TYPE(DT(4,3))        :: V(10000), U(100)
  INTEGER         :: I

    V = Arg(DT(4,3)("123"), (/(DT(4,3)("321"), I=1,10000)/))

    DO I = 1, 10000
      IF (V(I)%Base%C .NE. "123")              STOP 11

      U = DT(4,3)("")
      U = V(I)%Proc((/(DT(4,3)("321"), I=1,100)/))
      IF (ANY(U%Base%C .NE. "123")    )          STOP 21

    END DO

  END SUBROUTINE

  SUBROUTINE IntSub2(Arg)
  PROCEDURE(IFun), POINTER :: Arg
  TYPE(DT(4,3))                 :: V(10000), U(100)
  INTEGER                  :: I

    V = Arg(DT(4,3)("123"),(/(DT(4,3)("321"), I=1,10000)/))

    DO I = 1, 10000

      IF (V(I)%Base%C .NE. "123")              STOP 31

      U = DT(4,3)("")
      U = V(I)%Proc((/(DT(4,3)("321"), I=1,100)/))
      IF (ANY(U%Base%C .NE. "123")    )          STOP 41

    END DO

  END SUBROUTINE


  END

