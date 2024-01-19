! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Assign4.f
! opt variations: -qnok -qnol -qnodeferredlp

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
!*  Defined assignment-array
!*  (314836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc1 => ModFun1
    END TYPE

    TYPE, EXTENDS(BAse) :: DT(K2,N2)    ! (4,20,4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc2 => ModFun2
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(Base(4,*)) :: Arg
    CLASS(Base(4,:)), POINTER :: ModFun1(:)
      ALLOCATE(ModFun1(3), SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT(4,*,4,*)) :: Arg
    CLASS(DT(4,:,4,:)), POINTER :: ModFun2(:)
      ALLOCATE(ModFun2(3), SOURCE=Arg)
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT(4,*,4,*)), INTENT (OUT) :: Arg1(:)
    TYPE(DT(4,*,4,*)), INTENT (IN)  :: Arg2(SIZE(Arg1))
      DO I=1, SIZE(Arg1)
        Arg1(I)%Base = Arg2(I)%Base
        Arg1(I)%ProcPtr2 =>  Arg2(I)%ProcPtr2
      END DO
    END SUBROUTINE

  END MODULE


  PROGRAM Assign4
  USE M
  IMPLICIT NONE

  TYPE (DT(4,20,4,20))            :: V(1023)
  TYPE (Base(4,20))               :: V1(1023)
  TYPE (DT(4,:,4,:)), ALLOCATABLE :: V2(:)
  TYPE (Base(4,:)), POINTER       :: V3(:)
  INTEGER                         :: I

  PROCEDURE(ModFun1), POINTER :: ProcPtr1
  PROCEDURE(ModFun2), POINTER :: ProcPtr2

  ProcPtr1 => ModFun1
  ProcPtr2 => ModFun2

  V = DT(4,20,4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )
  V1 = Base(4,20)(ModFun1)
  ALLOCATE(V2(1023), SOURCE=DT(4,20,4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 ))
  ALLOCATE(V3(1023), SOURCE=Base(4,20)(ProcPtr1) )

  DO I=1, 1023
    IF (.NOT. Equal(V(I),  DT(4,20,4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )) ) ERROR STOP 11
    IF (.NOT. Equal(V1(I), Base(4,20)(ProcPtr1)) )                             ERROR STOP 12
    IF (.NOT. Equal(V2(I), DT(4,20,4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )) ) ERROR STOP 13
    IF (.NOT. Equal(V3(I), Base(4,20)(ProcPtr1)) )                             ERROR STOP 14
  END DO

  CONTAINS

  FUNCTION Equal(Arg1, Arg2)
  LOGICAL Equal
  CLASS(Base(4,*)) :: Arg1, Arg2

  Equal = .FALSE.

  SELECT TYPE ( Arg1 )
  TYPE IS (Base(4,*))
    SELECT TYPE ( Arg2 )
    TYPE IS (Base(4,*) )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1)
    CLASS DEFAULT
      STOP 22
    END SELECT
  TYPE IS (DT(4,*,4,*))
    SELECT TYPE ( Arg2 )
    TYPE IS (DT(4,*,4,*) )
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

