! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/StrComp10.f
! opt variations: -qnock -qnok -qnol -qdefaultpv

! *********************************************************************
!*  ===================================================================
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

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    TYPE  :: DT(K2,N2,K3,N3)    ! (4,20,1,3)
      INTEGER, KIND     :: K2,K3
      INTEGER, LEN      :: N2,N3
      TYPE(Base(K3,N3)) :: BComp
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CHARACTER(3) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM StrComp10
  USE M
  IMPLICIT TYPE(DT(4,20,1,3))(P)

  INTEGER :: I
  PROCEDURE(CHARACTER(3)),  POINTER :: ProcPtr=>NULL()
  TYPE(DT(4,20,1,3)) :: V(512)=(/(DT(4,20,1,3)(Base(1,3)("123",NULL())), I=1, 512)/)
  TYPE(DT(4,20,1,3)) :: U(SIZE(V)), W(SIZE(V))

  DO I=1, 512
    IF ( ASSOCIATED(V(I)%BComp%ProcPtr)) ERROR STOP 11
  END DO

  ProcPtr => ModFun
  V = (/(DT(4,20,1,3)(Base(1,3)("123", ModFun)), I=1, 512)/)

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(V(I)%BComp%ProcPtr, ModFun)) ERROR STOP 21
  END DO

  U = V

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(U(I)%BComp%ProcPtr, ModFun)) ERROR STOP 21
  END DO

  WHERE ((/(.TRUE._2, i=1, 512)/))
    W = V
  END WHERE

  DO I=1, 512
    IF ( .NOT. ASSOCIATED(W(I)%BComp%ProcPtr, ModFun)) ERROR STOP 21
  END DO

  END

