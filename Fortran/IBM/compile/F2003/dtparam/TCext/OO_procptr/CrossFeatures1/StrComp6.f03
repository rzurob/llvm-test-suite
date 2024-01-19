! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_procptr/CrossFeatures1/StrComp6.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!*  A data-target shall correspond to a nonprocedure pointer component;
!*  a proc-target shall correspond to a procedure pointer component
!*  (Err Msg wrong)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(K2,N2)    ! (4,20)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N2
      TYPE(Base(K2,N2))         :: BaseComp=Base(K2,20)(NULL())
      TYPE(Base(K2,:)), POINTER :: BasePtr=>NULL()
      PROCEDURE()    , NOPASS, POINTER :: ProcPtr1=>NULL()
      PROCEDURE(TYPE(Base(4,20))), NOPASS, POINTER :: ProcPtr2=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base(4,*)) :: Arg
    TYPE(Base(4,20)) :: ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM StrComp6
  USE M
  IMPLICIT NONE

  TYPE(DT(4,20)), TARGET  :: DTTar
  TYPE(DT(4,:)), POINTER :: DTPtr
  PROCEDURE(TYPE(DT(4,20))),  POINTER :: ProcPtr=>NULL()


  TYPE(DT(4,20)) :: V

  V = DT(4,20)(ModFun, NULL(), NULL(), NULL() )
  V = DT(4,20)(Base(4,20)(NULL()), ModFun, NULL(), NULL())
  V = DT(4,20)(Base(4,20)(NULL()), NULL(),DTTar, NULL() )
  V = DT(4,20)(Base(4,20)(NULL()), NULL(), NULL(), DTPtr)

  V = DT(4,20)(Base(4,20)(ModFun), NULL(), NULL(), NULL())

  END

