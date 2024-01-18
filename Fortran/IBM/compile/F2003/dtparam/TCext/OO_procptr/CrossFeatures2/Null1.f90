! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/Null1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 10, 2005
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
!*   null()
!*   The characteristics
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT(4)) :: Fun
    CLASS(DT(4)) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null1
  USE M
  IMPLICIT NONE

  TYPE (DT(4)) :: V

  INTERFACE
    TYPE(DT(4)) FUNCTION ExtFun()
    IMPORT DT
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
  PROCEDURE(ExtFun), POINTER :: ProcPtr1=>NULL()

  ProcPtr1 => ExtFun

  ProcPtr => ExtFun
  ProcPtr => NULL(ProcPtr)
  ProcPtr => NULL(ProcPtr1)


  V%ProcPtr => NULL(Fun)
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr1)

  ProcPtr1 => Fun   ! This is wrong
  ProcPtr1 => NULL(ProcPtr)


  END

