! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Assign5.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 16, 2005
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
!*  A derived-type intrinsic assignment
!*  304717->305713
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
        CHARACTER(*) :: Arg(:)
        CHARACTER(LEN(Arg)) :: CToC(SIZE(Arg))
      END FUNCTION
    END INTERFACE

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(N2,K2)    ! (20,4)
      INTEGER, KIND                 :: K2
      INTEGER, LEN                  :: N2
      INTEGER(K2)                   :: Id
      TYPE(Base(K2,:)), ALLOCATABLE :: BComp
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg(:)
    CHARACTER(LEN(Arg)) :: Fun(SIZE(Arg))
      Fun = Arg
    END FUNCTION

    RECURSIVE FUNCTION SavePtr(Arg1, Arg2) RESULT(Res)
    PROCEDURE(CToC)                :: Arg1
    CHARACTER(*)                   :: Arg2(:)
    PROCEDURE(CToC), POINTER, SAVE :: ProcPtr=>NULL()
    INTEGER,                  SAVE :: Count=5
    CHARACTER(LEN(Arg2))           :: Res(SIZE(Arg2))

      Count = Count - 1
      ProcPtr => Arg1

      IF (Count .NE. 0 ) THEN
        Res=SavePtr(Arg1,Arg2)
      ELSE
        IF ( .NOT. ASSOCIATED(ProcPtr, Arg1) ) THEN
          Res=""
        ELSE
          Res=Arg1(Arg2)
          Count = 5 ! restore the init
        END IF
      END IF

    END FUNCTION


  END MODULE


  PROGRAM Assign5
  USE M
  IMPLICIT NONE

  TYPE (DT(20,4)) :: V
  PROCEDURE(SavePtr), POINTER :: ProcPtr
  CHARACTER(1025) :: Str=CHAR(40)

  ALLOCATE(base(4,20) :: V%BComp)
  V%BComp = Base(4,20)(RetPtr(Fun))

  IF ( .NOT. ALLOCATED(V%BComp) )  STOP 12
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, RetPtr(Fun)) ) STOP 13

  ProcPtr => SavePtr
  IF (ANY(ProcPtr(Fun, (/"123", "abc"/)) .NE. (/"123", "abc"/)) ) STOP 14
  IF (ANY(ProcPtr(Fun, (/"", ""/))       .NE. (/"", ""/)) )       STOP 15


  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

