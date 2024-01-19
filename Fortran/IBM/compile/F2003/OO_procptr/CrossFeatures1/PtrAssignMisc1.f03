! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 26, 2005
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
!*  Data target in procedure pointer assignment
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION ExtFun()
  INTEGER, POINTER :: ExtFun
    !ALLOCATE(ExtFun, SOURCE=-1)
    ALLOCATE(ExtFun)
    ExtFun = -1
  END FUNCTION

  PROGRAM PtrAssignMisc1
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1()
      INTEGER, POINTER :: F1
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1),      POINTER :: ProcPtr1

  INTERFACE
    FUNCTION ExtFun()
      INTEGER, POINTER :: ExtFun
    END FUNCTION
  END INTERFACE


  PROCEDURE(INTEGER), POINTER :: ProcPtr2
  INTEGER,            POINTER :: IntPtr

  PROCEDURE(INTEGER), POINTER :: ProcPtr3
  INTEGER,            TARGET  :: IntTar


  ProcPtr1 => ExtFun()

  ProcPtr1 => IntPtr

  ProcPtr1 => IntTar

  ProcPtr2 => IntPtr

  ProcPtr3 => IntTar


  END

