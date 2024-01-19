! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/OO_procptr/CrossFeatures1/PtrAssignImp.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
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
!*  Implicit interface and explicitly typed
!*  or referenced as a function
!*  (304482)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 1
    END TYPE

    TYPE :: Child(K2,N2)    ! (4,20)
      INTEGER, KIND     :: K2
      INTEGER, LEN      :: N2
      TYPE(Base(N2,K2)) :: BaseComp
    END TYPE

  END MODULE

  FUNCTION ExtFun()
  USE M
  TYPE (Child(4,20)) :: ExtFun
    ExtFun = Child(4,20)(Base(20,4)(Id=-1))
  END FUNCTION

  PROGRAM PtrAssignImp
  USE M
  IMPLICIT TYPE(Child(4,20))(C)

  INTERFACE
    FUNCTION ExtFun()
     IMPORT Child
     TYPE (Child(4,20)) :: ExtFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(TYPE(Child(4,20))),   POINTER :: ProcPtr
  TYPE (Child(4,20)) :: V

  PROCEDURE(),  POINTER :: CProcPtr
  TYPE (Child(4,20)) :: W

  ProcPtr => ExtFun
  V = ProcPtr()
  IF ( V%BaseComp%ID  .NE. -1 ) ERROR STOP 11

  CProcPtr => ExtFun
  W = CProcPtr()
  IF ( V%BaseComp%ID  .NE. -1 ) ERROR STOP 12

  END

