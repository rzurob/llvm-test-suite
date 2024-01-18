! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/CrossFeatures2/PtrAssignGen.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignGen.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignGen.f
!*
!*  DATE                       : Mar. 27, 2005
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
!*  Generic interface
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: DT0(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: ID0 = 0
    END TYPE

    TYPE, EXTENDS(DT0) :: DT1    ! (20,4)
      INTEGER(K1) :: ID1 = -1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT2    ! (20,4)
      INTEGER(K1) :: ID2 = -2
    END TYPE

    TYPE, EXTENDS(DT2) :: DT3    ! (20,4)
      INTEGER(K1) :: ID3 = -3
    END TYPE

    CONTAINS

    FUNCTION ModDt0(Arg)
    TYPE(DT0(20,4)) :: ModDt0, Arg
      ModDt0 = Arg
    END FUNCTION

    FUNCTION ModDt1(Arg)
    TYPE(DT1(20,4)) :: ModDt1, Arg
      ModDt1 = Arg
    END FUNCTION

    FUNCTION ModDt2(Arg)
    TYPE(DT2(*,4)) :: Arg
    CLASS(DT2(:,4)), POINTER :: ModDt2
      ALLOCATE(ModDt2, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModDt3(Arg)
    CLASS(DT3(:,4)), ALLOCATABLE :: ModDt3
    CLASS(DT3(*,4))  :: Arg
      ALLOCATE(ModDt3, SOURCE=Arg)
    END FUNCTION

  END MODULE

  MODULE M1
  USE M

  INTERFACE ModDt1
    MODULE PROCEDURE  ModDt0
    MODULE PROCEDURE  ModDt1
    MODULE PROCEDURE  ModDt2
    MODULE PROCEDURE  ModDt3
  END INTERFACE

  END MODULE

  PROGRAM PtrAssignGen
  USE M1
  IMPLICIT NONE

  INTERFACE ModDt2
    MODULE PROCEDURE  ModDt0
    MODULE PROCEDURE  ModDt1
    MODULE PROCEDURE  ModDt2
    MODULE PROCEDURE  ModDt3
  END INTERFACE

  PROCEDURE(ModDt1),   POINTER :: ProcPtr1
  PROCEDURE(ModDt2),   POINTER :: ProcPtr2

  ProcPtr1 => ModDt1
  ASSOCIATE ( As => ProcPtr1(DT1(20,4)(ID1=1)) )
    IF ( As%Id1 .NE. 1 ) STOP 11
  END ASSOCIATE

  ProcPtr2 => ModDt2
  ASSOCIATE ( As => ProcPtr2(DT2(20,4)(ID2=2)) )
    IF ( As%Id2 .NE. 2 ) STOP 12
  END ASSOCIATE

  ASSOCIATE ( As => ProcPtr2(DT2(20,4)(Id1=1, Id2=2)) )
    SELECT TYPE ( As)
    TYPE IS (DT2(*,4))
      IF ( As%Id1 .NE. 1 ) STOP 13
      IF ( As%Id2 .NE. 2 ) STOP 14
    CLASS DEFAULT
      STOP 33
    END SELECT
  END ASSOCIATE


  END

