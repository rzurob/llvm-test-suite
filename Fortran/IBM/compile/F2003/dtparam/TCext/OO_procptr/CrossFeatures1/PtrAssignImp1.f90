! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/PtrAssignImp1.f
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
!*  ()
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

  SUBROUTINE ExtSub(Arg)
  USE M
  TYPE (Child(4,*)) :: Arg
    Arg = Child(4,20)(Base(20,4)(-1))
  END SUBROUTINE

  PROGRAM PtrAssignImp1
  USE M
  IMPLICIT TYPE(Child(4,20))(C)

  INTERFACE
    SUBROUTINE ExtSub(Arg)
     IMPORT Child
     TYPE (Child(4,*)) :: Arg
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(TYPE(Child(4,20))),  POINTER :: ProcPtr
  PROCEDURE(),             POINTER :: CProcPtr

  ProcPtr => ExtSub
  CProcPtr => ExtSub
  PRINT*, CProcPtr(Child(4,20)(Base(20,4)(1)))
  END

