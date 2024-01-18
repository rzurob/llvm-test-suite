! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 29, 2005
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
!*  Common block -
!*  A procedure pointer shall be storage associated only with another
!*  procedure pointer; either both interfaces shall be explicit or
!*  both interfaces shall be implicit. If the interfaces are explicit,
!*  the characteristics shall be the same. If the interfaces are
!*  mplicit, either both shall be subroutines or both shall be
!*  functions with the same type and type parameters.
!*  - on implicit interface
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  INTERFACE
    FUNCTION Fun()
      INTEGER :: Fun
    END FUNCTION
  END INTERFACE

  PROCEDURE(INTEGER), POINTER :: ProcPtr1
  PROCEDURE(INTEGER), POINTER :: ProcPtr2

  COMMON ProcPtr1
  COMMON /blk/ ProcPtr2

  INTERFACE
    FUNCTION Fun1()
      IMPORT Fun
      PROCEDURE(Fun), POINTER :: Fun1
    END FUNCTION
  END INTERFACE

  CONTAINS

  FUNCTION ModFun()
  INTEGER :: ModFun
    ModFun = -1
  END FUNCTION

  END MODULE

  FUNCTION ExtFun()
  INTEGER :: ExtFun
    ExtFun = -2
  END FUNCTION

  FUNCTION ExtFun1()
  USE M, ONLY: Fun
  PROCEDURE(Fun), POINTER :: ProcPtr
  PROCEDURE(Fun), POINTER :: ExtFun1
  COMMON ProcPtr
    ExtFun1 => ProcPtr
  END FUNCTION

  FUNCTION ExtFun2()
  USE M, ONLY: Fun
  PROCEDURE(Fun), POINTER :: ProcPtr
  PROCEDURE(Fun), POINTER :: ExtFun2
  COMMON /blk/ ProcPtr
    ExtFun2 => ProcPtr
  END FUNCTION


  PROGRAM Common3
  USE M

  PROCEDURE(Fun)              :: ExtFun
  PROCEDURE(Fun1)             :: ExtFun1, ExtFun2
  PROCEDURE(INTEGER), POINTER :: ProcPtr

  ProcPtr1 => ModFun
  ProcPtr  => ExtFun1()
  IF ( .NOT. ASSOCIATED( ProcPtr, ModFun ) ) STOP 11
  IF ( ProcPtr() .NE. -1 )                   STOP 12

  ProcPtr2 => ExtFun
  ProcPtr  => ExtFun2()
  IF ( .NOT. ASSOCIATED( ProcPtr, ExtFun ) ) STOP 21
  IF ( ProcPtr() .NE. -2 )                   STOP 22

  END

