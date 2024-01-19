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
  CLASS(*), POINTER :: ExtFun
    ALLOCATE(ExtFun, SOURCE=-1)
  END FUNCTION

  PROGRAM PtrAssignMisc2
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1()
      CLASS(*), POINTER :: F1
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1
  PROCEDURE(F1)          :: ExtFun

  CLASS(*), POINTER              :: UnlPtr
  CLASS(*), ALLOCATABLE, TARGET  :: UnlTar

  PROCEDURE(ProcPtr1), POINTER :: ProcPtr2
! PROCEDURE(CLASS(*)), POINTER :: ProcPtr3  ! this is wrong too
  PROCEDURE(),         POINTER :: ProcPtr4

  ProcPtr1 => ExtFun()
  ProcPtr1 => UnlPtr
  ProcPtr1 => UnlTar

  ProcPtr2 => ExtFun()
  ProcPtr2 => UnlPtr
  ProcPtr2 => UnlTar

  ProcPtr4 => ExtFun()
  ProcPtr4 => UnlPtr
  ProcPtr4 => UnlTar

  END


