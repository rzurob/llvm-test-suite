! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 12, 2005
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
!*  A variable that appears in a DATA statement and has not been typed previously
!*  may appear in a subsequent type declaration only if that declaration
!*  confirms the implicit typing
!*
!*  (ICE-305762)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Data1
  IMPLICIT CHARACTER(3)(C)
  IMPLICIT TYPE(DT)(V)

  TYPE :: DT
    INTEGER :: Id
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  DATA ProcPtr /NULL()/
  PROCEDURE(), POINTER :: ProcPtr

  DATA CProcPtr /NULL()/
  PROCEDURE(CHARACTER(3)), POINTER :: CProcPtr

  DATA V /DT(-1, NULL())/ !fine
  DATA V1 /3*DT(-1, NULL())/ !?
  TYPE (DT) :: V, V1(3)

  DATA W%ProcPtr /NULL()/
  TYPE(DT) :: W

  END


