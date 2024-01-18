! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/PtrAssignProcNameProcPtrComp.f
! opt variations: -qnol -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 19, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  The target is a procedure pointer component
!*  (300958)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
      PROCEDURE(IFun), NOPASS, POINTER :: BasePtr=>NULL()
      PROCEDURE(IFunArr), NOPASS, POINTER :: BasePtrArr=>NULL()
    END TYPE

    TYPE :: Child(N2,K2)    ! (20,4)
      INTEGER, KIND     :: K2
      INTEGER, LEN      :: N2
      TYPE(Base(N2,K2)) :: BaseComp
      INTEGER(K2)       :: ChildId = 2
      PROCEDURE(IFun), NOPASS, POINTER :: ChildPtr=>NULL()
      PROCEDURE(IFunArr), NOPASS, POINTER :: ChildPtrArr=>NULL()
    END TYPE

    INTERFACE
      FUNCTION IFun(Arg)
        LOGICAL(8)          :: Arg
        LOGICAL(8), POINTER :: IFun
      END FUNCTION
      FUNCTION IFunArr(Arg)
        LOGICAL(8)          :: Arg(:)
        LOGICAL(8), POINTER :: IFunArr(:)
      END FUNCTION
    END INTERFACE

  END MODULE

  FUNCTION ExtFun(Arg)
    LOGICAL(8)          :: Arg
    LOGICAL(8), POINTER :: ExtFun
      !ALLOCATE(ExtFun, SOURCE=Arg)
      ALLOCATE(ExtFun)
      ExtFun = Arg
  END FUNCTION

  FUNCTION ExtFunArr(Arg)
    LOGICAL(8)          :: Arg(:)
    LOGICAL(8), POINTER :: ExtFunArr(:)
      !ALLOCATE(ExtFunArr(SIZE(Arg)), SOURCE=Arg) !not 10.1
      ALLOCATE(ExtFunArr(SIZE(Arg)))
      ExtFunArr = Arg
  END FUNCTION


  PROGRAM PtrAssignProcNameProcPtrComp
  USE M
  IMPLICIT NONE

  TYPE(Child(20,4))                 :: V
  PROCEDURE(IFun),    POINTER :: Ptr1
  PROCEDURE(IFun)             :: ExtFun
  PROCEDURE(IFunArr), POINTER :: Ptr2
  PROCEDURE(IFunArr)          :: ExtFunArr

  V%BaseComp%BasePtr  => ExtFun
  Ptr1 => V%BaseComp%BasePtr
  IF ( Ptr1(.FALSE._8 ) ) STOP 11

  V%ChildPtr => ExtFun
  Ptr1 => V%ChildPtr
  IF ( .NOT. Ptr1(.TRUE._8) ) STOP 12

  V%BaseComp%BasePtrArr  => ExtFunArr
  Ptr2 => V%BaseComp%BasePtrArr
  IF ( ANY(Ptr2((/.FALSE._8, .TRUE._8, .FALSE._8/)) .NEQV. &
              & (/.FALSE._8, .TRUE._8, .FALSE._8/) ) ) STOP 11

  V%ChildPtrArr => ExtFunArr
  Ptr2 => V%ChildPtrArr
  IF ( ANY(Ptr2((/.FALSE._8, .TRUE._8, .FALSE._8/)) .NEQV. &
              & (/.FALSE._8, .TRUE._8, .FALSE._8/) ) ) STOP 11

  END

